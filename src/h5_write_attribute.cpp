#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"

#include <cstddef>
#include <algorithm>
#include <cstring>

//[[Rcpp::export(rng=false)]]
SEXP h5_write_attribute(Rcpp::RObject aptr, Rcpp::RObject vec) {
    auto& ahandle = extract_attribute(aptr);

    auto aspace = ahandle.getSpace();
    std::vector<hsize_t> dims;
    const auto ndims = aspace.getSimpleExtentNdims();
    if (ndims) {
        sanisizer::resize(dims, ndims);
        aspace.getSimpleExtentDims(dims.data());
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
        ahandle.write(H5::PredType::NATIVE_INT, static_cast<const int*>(ivec.begin()));

    } else if (vec.sexp_type() == LGLSXP) {
        Rcpp::LogicalVector lvec(vec);
        if (!sanisizer::is_equal(len, lvec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        ahandle.write(H5::PredType::NATIVE_INT, static_cast<const int*>(lvec.begin()));

    } else if (vec.sexp_type() == REALSXP) {
        Rcpp::NumericVector dvec(vec);
        if (!sanisizer::is_equal(len, dvec.size())) {
            throw std::runtime_error("dataset and vector lengths are not consistent");
        }
        ahandle.write(H5::PredType::NATIVE_DOUBLE, static_cast<const double*>(dvec.begin()));

    } else if (vec.sexp_type() == STRSXP) {
        auto stype = ahandle.getStrType();
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
            // Populating the vector of Rcpp::Strings first, just in case c_string() references an internal buffer of the Rcpp::String.
            // Otherwise, we might end up with dangling pointers when the Rcpp::String is ultimately destroyed.
            std::vector<Rcpp::String> block_strs;
            block_strs.reserve(len);
            for (hsize_t s = 0; s < len; ++s) {
                block_strs.push_back(svec[s]);
            }
            std::vector<const char*> block_ptrs;
            block_ptrs.reserve(len);
            for (hsize_t s = 0; s < len; ++s) {
                block_ptrs.push_back(block_strs[s].get_cstring());
            }
            ahandle.write(stype, block_ptrs.data());

        } else {
            const auto elsize = stype.getSize();
            std::vector<char> block_buffer(sanisizer::product<typename std::vector<char>::size_type>(elsize, len));
            for (hsize_t s = 0; s < len; ++s) {
                Rcpp::String curstr(svec[s]);
                const auto ptr = curstr.get_cstring();
                std::copy_n(
                    ptr,
                    sanisizer::min(elsize, std::strlen(ptr)),
                    block_buffer.data() + sanisizer::product_unsafe<std::size_t>(s, elsize)
                );
            }
            ahandle.write(stype, block_buffer.data());
        }

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return R_NilValue;
}
