#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"

#include <cstddef>
#include <algorithm>
#include <cstring>

//[[Rcpp::export(rng=false)]]
SEXP h5_write_vector(Rcpp::RObject dptr, Rcpp::RObject vec, std::size_t block_size_bytes) {
    auto& dhandle = extract_dataset(dptr);

    auto dspace = dhandle.getSpace();
    if (dspace.getSimpleExtentNdims() != 1) {
        throw std::runtime_error("dataset should be 1-dimensional");
    }
    hsize_t len;
    dspace.getSimpleExtentDims(&len);

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

        if (stype.isVariableStr()) {
            constexpr auto elsize = sizeof(Rcpp::String) + sizeof(char*);
            const auto bsize = sanisizer::max(1, block_size_bytes / elsize);
            std::vector<Rcpp::String> block_strs;
            std::vector<const char*> block_ptrs;
            block_strs.reserve(bsize);
            block_ptrs.reserve(bsize);

            hsize_t offset = 0;
            H5::DataSpace src;
            while (offset < len) {
                const hsize_t count = sanisizer::min(len - offset, bsize);

                block_strs.clear();
                for (hsize_t s = 0; s < count; ++s) {
                    block_strs.push_back(svec[offset + s]);
                }

                block_ptrs.clear();
                for (hsize_t s = 0; s < count; ++s) {
                    block_ptrs.push_back(block_strs[s].get_cstring());
                }

                src.setExtentSimple(1, &count);
                src.selectAll();
                dspace.selectHyperslab(H5S_SELECT_SET, &count, &offset);
                dhandle.write(block_ptrs.data(), stype, src, dspace);

                offset += count;
            }

        } else {
            const auto elsize = stype.getSize();
            const auto bsize = sanisizer::max(1, block_size_bytes / elsize);
            auto block_buffer = sanisizer::create<std::vector<char> >(sanisizer::product_unsafe<std::size_t>(bsize, elsize));

            hsize_t offset = 0;
            H5::DataSpace src;
            while (offset < len) {
                const hsize_t count = sanisizer::min(len - offset, bsize);

                for (hsize_t s = 0; s < count; ++s) {
                    Rcpp::String curstr(svec[s]);
                    const auto ptr = curstr.get_cstring();
                    std::copy_n(
                        ptr,
                        sanisizer::min(elsize, std::strlen(ptr)),
                        block_buffer.data() + sanisizer::product_unsafe<std::size_t>(s, elsize)
                    );
                }

                src.setExtentSimple(1, &count);
                src.selectAll();
                dspace.selectHyperslab(H5S_SELECT_SET, &count, &offset);
                dhandle.write(block_buffer.data(), stype, src, dspace);

                offset += count;
            }
        }

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return R_NilValue;
}
