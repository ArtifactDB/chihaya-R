#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"
#include "VlenReclaimer.h"
#include "cast_to_double.h"

#include <cstddef>
#include <algorithm>
#include <cstdint>

//[[Rcpp::export(rng=false)]]
Rcpp::List h5_read_attribute(Rcpp::RObject aptr) {
    auto& ahandle = extract_attribute(aptr);

    auto aspace = ahandle.getSpace();
    std::vector<hsize_t> dims;
    const auto ndims = aspace.getSimpleExtentNdims();
    if (ndims) {
        sanisizer::resize(dims, ndims);
        aspace.getSimpleExtentDims(dims.data());
    }

    auto atype = ahandle.getDataType();
    auto aclass = atype.getClass();

    Rcpp::List output;
    R_xlen_t len = 1; // defaults to length 1 if scalar, i.e., ndims == 0.
    Rcpp::IntegerVector outdims(ndims);
    for (I<decltype(ndims)> d = 0; d < ndims; ++d) {
        len = sanisizer::product<R_xlen_t>(len, dims[d]);
        outdims[d] = sanisizer::cast<int>(dims[d]);
    }
    output["dims"] = outdims;

    if (aclass == H5T_INTEGER) {
        auto itype = ahandle.getIntType(); 
        auto dsize = itype.getSize();
        auto dsign = itype.getSign(); 

        if (dsize < 4 || (dsize == 4 && dsign == H5T_SGN_2)) {
            auto ivec = sanisizer::create<Rcpp::IntegerVector>(len);
            ahandle.read(H5::PredType::NATIVE_INT, static_cast<int*>(ivec.begin()));
            output["value"] = ivec;

        } else {
            auto dvec = sanisizer::create<Rcpp::NumericVector>(len);

            // If it's too large for a 32-bit integer, we try to convert it to double-precision.
            if (dsize == 4 && dsign == H5T_SGN_NONE) {
                auto tmpvec = sanisizer::create<std::vector<std::uint32_t> >(len);
                ahandle.read(H5::PredType::NATIVE_UINT32, tmpvec.data());
                cast_to_double(tmpvec, dvec.begin());
            } else if (dsize <= 8 && dsign == H5T_SGN_2) {
                auto tmpvec = sanisizer::create<std::vector<std::int64_t> >(len);
                ahandle.read(H5::PredType::NATIVE_INT64, tmpvec.data());
                cast_to_double(tmpvec, dvec.begin());
            } else if (dsize <= 8 && dsign == H5T_SGN_NONE) {
                auto tmpvec = sanisizer::create<std::vector<std::uint64_t> >(len);
                ahandle.read(H5::PredType::NATIVE_UINT64, tmpvec.data());
                cast_to_double(tmpvec, dvec.begin());
            } else {
                throw std::runtime_error("unsupported HDF5 integer type");
            }

            output["value"] = dvec;
        }

    } else if (aclass == H5T_FLOAT) {
        auto dvec = sanisizer::create<Rcpp::NumericVector>(len);
        ahandle.read(H5::PredType::NATIVE_DOUBLE, static_cast<double*>(dvec.begin()));
        output["value"] = dvec;
        output["type"] = "float";

    } else if (aclass == H5T_STRING) {
        auto stype = ahandle.getStrType();
        const auto enc = (stype.getCset() == H5T_CSET_ASCII ? CE_ANY : CE_UTF8);
        auto svec = sanisizer::create<Rcpp::StringVector>(len);

        if (stype.isVariableStr()) {
            auto block_ptrs = sanisizer::create<std::vector<const char*> >(len);
            ahandle.read(stype, block_ptrs.data());
            VlenReclaimer reclaimer(block_ptrs.data(), stype, aspace);
            for (I<decltype(len)> i = 0; i < len; ++i) {
                svec[i] = Rcpp::String(block_ptrs[i], enc);
            }

        } else {
            const auto elsize = stype.getSize();
            auto block_buffer = sanisizer::create<std::vector<char> >(len);
            auto tmp_buffer = sanisizer::create<std::vector<char> >(sanisizer::sum<std::size_t>(elsize, 1));
            ahandle.read(stype, block_buffer.data());
            for (I<decltype(len)> i = 0; i < len; ++i) {
                std::copy_n(
                    block_buffer.data() + sanisizer::product_unsafe<std::size_t>(i, elsize),
                    elsize,
                    tmp_buffer.begin()
                );
                svec[i] = Rcpp::String(tmp_buffer.data(), enc);
            }
        }

        output["value"] = svec;

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return output;
}
