#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"

#include <cstddef>
#include <algorithm>
#include <cstring>

H5T_conv_ret_t handle_conversion_error(H5T_conv_except_t, hid_t, hid_t, void*, void*, void* user_data) {
    *reinterpret_cast<int*>(user_data) = 1;
    return H5T_CONV_HANDLED;
}

//[[Rcpp::export(rng=false)]]
SEXP h5_read_vector(Rcpp::RObject dptr, std::size_t block_size_bytes) {
    auto& dhandle = extract_dataset(dptr);

    auto dspace = dhandle.getSpace();
    if (dspace.getSimpleExtentNdims() != 1) {
        throw std::runtime_error("dataset should be 1-dimensional");
    }
    hsize_t len;
    dspace.getSimpleExtentDims(&len);

    auto dtype = dhandle.getDataType();
    auto dclass = dtype.getClass();

    if (dclass == H5T_INTEGER) {
        H5::IntType itype(dhandle); 
        auto dsize = itype.getSize();
        auto dsign = itype.getSign(); 

        if (dsize < 4 || (dsize == 4 && dsign == H5T_SGN_2)) {
            auto ivec = sanisizer::create<Rcpp::IntegerVector>(len);
            dhandle.read(static_cast<int*>(ivec.begin()), H5::PredType::NATIVE_INT);
            return ivec;
        }

        // If it's too large for a 32-bit integer, we try to convert it to double-precision.
        auto dvec = sanisizer::create<Rcpp::NumericVector>(len);
        H5::DSetMemXferPropList xplist(H5::DSetMemXferPropList::DEFAULT);
        int failed = 0;
        xplist.setTypeConvCB(&handle_conversion_error, &failed);

        dhandle.read(
            static_cast<double*>(dvec.begin()),
            H5::PredType::NATIVE_DOUBLE,
            dspace,
            dspace,
            xplist
        );
        if (failed) {
            throw std::runtime_error("cannot accurately convert large integers to double-precision");
        }
        return dvec;

    } else if (dclass == H5T_FLOAT) {
        auto dvec = sanisizer::create<Rcpp::NumericVector>(len);
        dhandle.read(static_cast<double*>(dvec.begin()), H5::PredType::NATIVE_DOUBLE);
        return dvec;

    } else if (dclass == H5T_STRING) {
        H5::StrType stype(dhandle);
        const auto enc = (stype.getCset() == H5T_CSET_ASCII ? CE_ANY : CE_UTF8);
        auto svec = sanisizer::create<Rcpp::StringVector>(len);

        if (stype.isVariableStr()) {
            constexpr auto elsize = sizeof(char*);
            const auto bsize = sanisizer::max(1, block_size_bytes / elsize);
            std::vector<const char*> block_ptrs;
            block_ptrs.reserve(bsize);

            hsize_t offset = 0;
            H5::DataSpace src;
            H5::DSetMemXferPropList xplist(H5::DSetMemXferPropList::DEFAULT);
            while (offset < len) {
                const hsize_t count = sanisizer::min(len - offset, bsize);

                src.setExtentSimple(1, &count);
                src.selectAll();
                dspace.selectHyperslab(H5S_SELECT_SET, &count, &offset);
                dhandle.write(block_ptrs.data(), stype, src, dspace, xplist);
                
                try {
                    for (hsize_t i = 0; i < count; ++i) {
                        svec[offset + i] = Rcpp::String(block_ptrs[i], enc);
                    }
                } catch (...) {
                    H5Treclaim(stype.getId(), src.getId(), xplist.getId(), block_ptrs.data());
                    throw;
                }

                H5Treclaim(stype.getId(), src.getId(), xplist.getId(), block_ptrs.data());
                offset += count;
            }

        } else {
            const auto elsize = stype.getSize();
            const auto bsize = sanisizer::max(1, block_size_bytes / elsize);
            auto block_buffer = sanisizer::create<std::vector<char> >(sanisizer::product_unsafe<std::size_t>(bsize, elsize));
            auto tmp_buffer = sanisizer::create<std::vector<char> >(sanisizer::sum<std::size_t>(elsize, 1));

            hsize_t offset = 0;
            H5::DataSpace src;
            while (offset < len) {
                const hsize_t count = sanisizer::min(len - offset, bsize);

                src.setExtentSimple(1, &count);
                src.selectAll();
                dspace.selectHyperslab(H5S_SELECT_SET, &count, &offset);
                dhandle.write(block_buffer.data(), stype, src, dspace);

                for (hsize_t i = 0; i < count; ++i) {
                    std::copy_n(
                        block_buffer.data() + sanisizer::product_unsafe<std::size_t>(i, elsize),
                        elsize,
                        tmp_buffer.begin()
                    );
                    svec[offset + i] = Rcpp::String(tmp_buffer.data(), enc);
                }

                offset += count;
            }
        }

        return svec;

    } else {
        throw std::runtime_error("unsupported vector type");
    }

    return R_NilValue;
}
