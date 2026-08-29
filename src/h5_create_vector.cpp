#include "Rcpp.h"
#include "H5Cpp.h"

#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_create_vector(
    Rcpp::RObject gptr,
    std::string name,
    Rcpp::List type,
    Rcpp::RObject len,
    int compress,
    Rcpp::RObject chunk_size
) {
    const auto dlen = extract_hsize_t(len);
    
    H5::DSetCreatPropList cplist(H5::DSetCreatPropList::DEFAULT);
    cplist.setFillTime(H5D_FILL_TIME_NEVER);
    H5Pset_obj_track_times(cplist.getId(), false);
    if (compress && dlen) {
        cplist.setShuffle();
        cplist.setDeflate(compress);
        const auto bsize = std::min(dlen, extract_hsize_t(chunk_size));
        if (bsize == 0) {
            throw std::runtime_error("chunk size should be positive");
        }
        cplist.setChunk(1, &bsize);
    }

    H5::Group& ghandle = extract_group(gptr);
    H5::DataSpace dspace(1, &dlen);
    auto dtype = extract_data_type(type);
    auto dptr = new H5::DataSet(ghandle.createDataSet(name, dtype, dspace, cplist));
    return Rcpp::XPtr<H5::DataSet>(dptr, true);
}
