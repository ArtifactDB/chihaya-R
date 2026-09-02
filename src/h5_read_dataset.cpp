#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"
#include "traverse_array_by_chunk.h"
#include "VlenReclaimer.h"
#include "cast_to_double.h"

#include <cstddef>
#include <algorithm>
#include <cstdint>

template<typename OuterLoop_, typename InnerLoop_>
void read_array_by_chunk(
    const std::vector<hsize_t>& dims,
    const std::vector<hsize_t>& chunkdims,
    OuterLoop_ outer_loop,
    InnerLoop_ inner_loop
) {
    traverse_array_by_chunk(
        dims,
        chunkdims,
        std::move(outer_loop),
        std::move(inner_loop),
        [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> void {}
    );
}

//[[Rcpp::export(rng=false)]]
Rcpp::List h5_read_dataset(Rcpp::RObject dptr) {
    auto& dhandle = extract_dataset(dptr);

    auto dspace = dhandle.getSpace();
    std::vector<hsize_t> dims;
    const auto ndims = dspace.getSimpleExtentNdims();
    if (ndims) {
        sanisizer::resize(dims, ndims);
        dspace.getSimpleExtentDims(dims.data());
    }

    auto dtype = dhandle.getDataType();
    auto dclass = dtype.getClass();

    Rcpp::List output;
    R_xlen_t len = 1; // defaults to length 1 if scalar, i.e., ndims == 0.
    Rcpp::IntegerVector outdims(ndims);
    for (I<decltype(ndims)> d = 0; d < ndims; ++d) {
        len = sanisizer::product<R_xlen_t>(len, dims[d]);
        outdims[d] = sanisizer::cast<int>(dims[d]);
    }
    output["dims"] = outdims;

    if (dclass == H5T_INTEGER) {
        H5::IntType itype(dhandle); 
        auto dsize = itype.getSize();
        auto dsign = itype.getSign(); 

        if (dsize < 4 || (dsize == 4 && dsign == H5T_SGN_2)) {
            auto ivec = sanisizer::create<Rcpp::IntegerVector>(len);
            dhandle.read(static_cast<int*>(ivec.begin()), H5::PredType::NATIVE_INT);
            output["value"] = ivec;

        } else {
            // If it's too large for a 32-bit integer, we try to convert it to double-precision.
            // We do this chunk by chunk to avoid allocating more memory than we need.
            auto dvec = sanisizer::create<Rcpp::NumericVector>(len);

            if (ndims) {
                if (len) {
                    auto chunkdims = extract_chunk_dims(dhandle.getCreatePlist(), dims);
                    const hsize_t chunklen = get_chunk_length(chunkdims);

                    if (dsize == 4 && dsign == H5T_SGN_NONE) {
                        auto tmpvec = sanisizer::create<std::vector<std::uint32_t> >(chunklen);
                        read_array_by_chunk(
                            dims,
                            chunkdims, 
                            [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> bool {
                                dhandle.read(tmpvec.data(), H5::PredType::NATIVE_UINT32, mem_space, file_space);
                                return true;
                            },
                            [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                                dvec[full_offset] = cast_to_double(tmpvec[chunk_offset]);
                            }
                        );

                    } else if (dsize <= 8 && dsign == H5T_SGN_2) {
                        auto tmpvec = sanisizer::create<std::vector<std::int64_t> >(chunklen);
                        read_array_by_chunk(
                            dims,
                            chunkdims, 
                            [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> bool {
                                dhandle.read(tmpvec.data(), H5::PredType::NATIVE_INT64, mem_space, file_space);
                                return true;
                            },
                            [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                                dvec[full_offset] = cast_to_double(tmpvec[chunk_offset]);
                            }
                        );

                    } else if (dsize <= 8 && dsign == H5T_SGN_NONE) {
                        auto tmpvec = sanisizer::create<std::vector<std::uint64_t> >(chunklen);
                        read_array_by_chunk(
                            dims,
                            chunkdims, 
                            [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> bool {
                                dhandle.read(tmpvec.data(), H5::PredType::NATIVE_UINT64, mem_space, file_space);
                                return true;
                            },
                            [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                                dvec[full_offset] = cast_to_double(tmpvec[chunk_offset]);
                            }
                        );

                    } else {
                        throw std::runtime_error("unsupported HDF5 integer type");
                    }
                }

            } else {
                // Much simpler logic for scalars.
                if (dsize == 4 && dsign == H5T_SGN_NONE) {
                    std::uint32_t tmp;
                    dhandle.read(&tmp, H5::PredType::NATIVE_UINT32);
                    dvec[0] = cast_to_double(tmp);
                } else if (dsize <= 8 && dsign == H5T_SGN_2) {
                    std::int64_t tmp;
                    dhandle.read(&tmp, H5::PredType::NATIVE_INT64);
                    dvec[0] = cast_to_double(tmp);
                } else if (dsize <= 8 && dsign == H5T_SGN_NONE) {
                    std::uint64_t tmp;
                    dhandle.read(&tmp, H5::PredType::NATIVE_UINT64);
                    dvec[0] = cast_to_double(tmp);
                } else {
                    throw std::runtime_error("unsupported HDF5 integer type");
                }
            }

            output["value"] = dvec;
        }

    } else if (dclass == H5T_FLOAT) {
        auto dvec = sanisizer::create<Rcpp::NumericVector>(len);
        dhandle.read(static_cast<double*>(dvec.begin()), H5::PredType::NATIVE_DOUBLE);
        output["value"] = dvec;

    } else if (dclass == H5T_STRING) {
        H5::StrType stype(dhandle);
        const auto enc = (stype.getCset() == H5T_CSET_ASCII ? CE_ANY : CE_UTF8);
        auto svec = sanisizer::create<Rcpp::StringVector>(len);

        if (ndims) {
            if (len) {
                // Here, the idea is to only load each chunk in at a time, and convert it to an R string accordingly. 
                auto chunkdims = extract_chunk_dims(dhandle.getCreatePlist(), dims);
                const hsize_t chunklen = get_chunk_length(chunkdims);

                if (stype.isVariableStr()) {
                    auto block_ptrs = sanisizer::create<std::vector<const char*> >(chunklen);
                    read_array_by_chunk(
                        dims,
                        chunkdims, 
                        [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> VlenReclaimer {
                            dhandle.read(block_ptrs.data(), stype, mem_space, file_space);
                            return VlenReclaimer(block_ptrs.data(), stype, mem_space);
                        },
                        [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                            svec[full_offset] = Rcpp::String(block_ptrs[chunk_offset], enc);
                        }
                    );

                } else {
                    const auto elsize = stype.getSize();
                    std::vector<char> block_buffer(sanisizer::product<typename std::vector<char>::size_type>(chunklen, elsize));
                    std::vector<char> tmp_buffer(sanisizer::sum<typename std::vector<char>::size_type>(elsize, 1));
                    read_array_by_chunk(
                        dims,
                        chunkdims, 
                        [&](const H5::DataSpace& file_space, const H5::DataSpace& mem_space) -> bool {
                            dhandle.read(block_buffer.data(), stype, mem_space, file_space);
                            return true;
                        },
                        [&](hsize_t chunk_offset, hsize_t full_offset) -> void {
                            std::copy_n(
                                block_buffer.data() + sanisizer::product_unsafe<std::size_t>(chunk_offset, elsize),
                                elsize,
                                tmp_buffer.begin()
                            );
                            svec[full_offset] = Rcpp::String(tmp_buffer.data(), enc);
                        }
                    );
                }
            }

        } else {
            // Of course, for scalar strings, life is much easier.
            H5::DataSpace dest(H5S_SCALAR);
            if (stype.isVariableStr()) {
                std::vector<const char*> block_ptrs(1);
                dhandle.read(block_ptrs.data(), stype, dest);
                VlenReclaimer rclm(block_ptrs.data(), stype, dest);
                svec[0] = Rcpp::String(block_ptrs[0], enc);
            } else {
                const auto elsize = stype.getSize();
                std::vector<char> block_buffer(sanisizer::sum<typename std::vector<char>::size_type>(elsize, 1));
                dhandle.read(block_buffer.data(), stype, dest);
                svec[0] = Rcpp::String(block_buffer.data(), enc);
            }
        }

        output["value"] = svec;

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return output;
}
