#include "Rcpp.h"
#include "H5Cpp.h"

#include "extract_dimensions.h"
#include "decode_data_type.h"
#include "utils.h"

#include <algorithm>
#include <string>
#include <stdexcept>

//[[Rcpp::export(rng=false)]]
SEXP h5_create_dataset(
    Rcpp::RObject gptr,
    std::string dname,
    Rcpp::List raw_type,
    Rcpp::RObject raw_dim,
    int compress,
    Rcpp::RObject raw_chunks,
    bool is_vlen_str
) {
    const auto dims = extract_dimensions(raw_dim);
    const auto ndim = dims.size();
    bool is_empty = false;
    for (auto dd : dims) {
        if (dd == 0) {
            is_empty = true;
            break;
        }
    }

    H5::DSetCreatPropList cplist;
    H5Pset_obj_track_times(cplist.getId(), false);
    // Not really sure, but VL datasets don't like it if you don't fill it.
    if (!is_vlen_str) {
        cplist.setFillTime(H5D_FILL_TIME_NEVER);
    }

    if (ndim > 0 && compress && !is_empty) {
        cplist.setShuffle();
        cplist.setDeflate(compress);
        auto chunkdims = extract_dimensions(raw_chunks);
        if (chunkdims.size() != ndim) {
            throw std::runtime_error("'raw_chunks' should have the same length as 'raw_dim'");
        }
        for (I<decltype(ndim)> d = 0; d < ndim; ++d) {
            chunkdims[d] = std::min(chunkdims[d], dims[d]);
        }
        cplist.setChunk(ndim, chunkdims.data());
    }

    H5::Group& ghandle = extract_group(gptr);
    auto dtype = decode_data_type(raw_type);

    H5::DataSpace dspace;
    if (ndim) {
        dspace = H5::DataSpace(sanisizer::cast<int>(ndim), dims.data());
    }

    auto dptr = new H5::DataSet(ghandle.createDataSet(dname, dtype, dspace, cplist));
    return Rcpp::XPtr<H5::DataSet>(dptr, true);
}
