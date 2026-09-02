#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"
#include "traverse_array_by_chunk.h"

#include <cstddef>
#include <algorithm>
#include <cstring>

template<typename InnerLoop_, typename OuterLoop_>
void write_array_by_chunk(
    const std::vector<hsize_t>& dims,
    const std::vector<hsize_t>& chunkdims,
    InnerLoop_ inner_loop,
    OuterLoop_ outer_loop
) {
    traverse_array_by_chunk(
        dims,
        chunkdims,
        [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> bool { return false; },
        std::move(inner_loop),
        std::move(outer_loop)
    );
}

//[[Rcpp::export(rng=false)]]
SEXP h5_write_dataset(Rcpp::RObject dptr, Rcpp::RObject vec) {
    auto& dhandle = extract_dataset(dptr);

    auto dspace = dhandle.getSpace();
    std::vector<hsize_t> dims;
    const auto ndims = dspace.getSimpleExtentNdims();
    if (ndims) {
        sanisizer::resize(dims, ndims);
        dspace.getSimpleExtentDims(dims.data());
    }

    R_xlen_t len = 1; // defaults to length 1 if scalar, i.e., ndims == 0.
    for (I<decltype(ndims)> d = 0; d < ndims; ++d) {
        len = sanisizer::product<R_xlen_t>(len, dims[d]);
    }

    if (vec.sexp_type() == INTSXP) {
        Rcpp::IntegerVector ivec(vec);
        if (!sanisizer::is_equal(len, ivec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        dhandle.write(static_cast<const int*>(ivec.begin()), H5::PredType::NATIVE_INT);

    } else if (vec.sexp_type() == LGLSXP) {
        Rcpp::LogicalVector lvec(vec);
        if (!sanisizer::is_equal(len, lvec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        dhandle.write(static_cast<const int*>(lvec.begin()), H5::PredType::NATIVE_INT);

    } else if (vec.sexp_type() == REALSXP) {
        Rcpp::NumericVector dvec(vec);
        if (!sanisizer::is_equal(len, dvec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        dhandle.write(static_cast<const double*>(dvec.begin()), H5::PredType::NATIVE_DOUBLE);

    } else if (vec.sexp_type() == STRSXP) {
        H5::StrType stype(dhandle);
        Rcpp::StringVector svec(vec);
        if (!sanisizer::is_equal(len, svec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        for (hsize_t s = 0; s < len; ++s) {
            if (svec[s] == NA_STRING) {
                throw std::runtime_error("missing strings should be replaced with a placeholder");
            }
        }

        if (ndims) {
            if (len) {
                // Here, the idea is to only load each chunk in at a time, and convert it to an R string accordingly. 
                auto chunkdims = extract_chunk_dims(dhandle.getCreatePlist(), dims);
                const hsize_t chunklen = get_chunk_length(chunkdims);

                if (stype.isVariableStr()) {
                    auto block_ptrs = sanisizer::create<std::vector<const char*> >(chunklen);
                    auto block_strs = sanisizer::create<std::vector<Rcpp::String> >(chunklen);
                    write_array_by_chunk(
                        dims,
                        chunkdims, 
                        [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                            // Storing a temporary to ensure that the C string pointers are valid.
                            block_strs[chunk_offset] = Rcpp::String(svec[full_offset]);
                            block_ptrs[chunk_offset] = block_strs[chunk_offset].get_cstring();
                        },
                        [&](const H5::DataSpace& src_space, const H5::DataSpace& dest_space) -> void {
                            dhandle.write(block_ptrs.data(), stype, dest_space, src_space);
                        }
                    );

                } else {
                    const auto elsize = stype.getSize();
                    std::vector<char> block_buffer(sanisizer::product<typename std::vector<char>::size_type>(chunklen, elsize));
                    write_array_by_chunk(
                        dims,
                        chunkdims, 
                        [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                            Rcpp::String current(svec[full_offset]);
                            const auto ptr = current.get_cstring();
                            std::copy_n(
                                ptr,
                                sanisizer::min(elsize, std::strlen(ptr)),
                                block_buffer.data() + sanisizer::product_unsafe<std::size_t>(chunk_offset, elsize)
                            );
                        },
                        [&](const H5::DataSpace& src_space, const H5::DataSpace& dest_space) -> void {
                            dhandle.write(block_buffer.data(), stype, dest_space, src_space);
                            std::fill(block_buffer.begin(), block_buffer.end(), 0);
                        }
                    );
                }
            }

        } else {
            // Much simpler for scalars.
            if (stype.isVariableStr()) {
                Rcpp::String current(svec[0]);
                std::vector<const char*> ptrs(1);
                ptrs[0] = current.get_cstring();
                dhandle.write(ptrs.data(), stype);

            } else {
                const auto elsize = stype.getSize();
                auto tmp_buffer = sanisizer::create<std::vector<char> >(elsize);
                Rcpp::String current(svec[0]);
                const auto ptr = current.get_cstring();
                std::copy_n(
                    ptr,
                    sanisizer::min(elsize, std::strlen(ptr)),
                    tmp_buffer.begin()
                );
                dhandle.write(tmp_buffer.data(), stype);
            }
        }

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return R_NilValue;
}
