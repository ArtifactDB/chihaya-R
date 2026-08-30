#include "Rcpp.h"
#include "H5Cpp.h"

#include "extract_dimensions.h"
#include "decode_data_type.h"
#include "utils.h"

#include <algorithm>
#include <string>
#include <stdexcept>

//[[Rcpp::export(rng=false)]]
SEXP h5_create_dataset_attribute(
    Rcpp::RObject dptr,
    std::string aname,
    Rcpp::List raw_type,
    Rcpp::RObject raw_dim
) {
    const auto dims = extract_dimensions(raw_dim);
    const auto ndim = dims.size();
    H5::DataSpace aspace;
    if (ndim) {
        aspace = H5::DataSpace(sanisizer::cast<int>(ndim), dims.data());
    }

    auto atype = decode_data_type(raw_type);
    auto& dhandle = extract_dataset(dptr); 
    auto aptr = new H5::Attribute(dhandle.createAttribute(aname, atype, aspace));
    return Rcpp::XPtr<H5::Attribute>(aptr, true);
}

//[[Rcpp::export(rng=false)]]
SEXP h5_create_group_attribute(
    Rcpp::RObject gptr,
    std::string aname,
    Rcpp::List raw_type,
    Rcpp::RObject raw_dim
) {
    const auto dims = extract_dimensions(raw_dim);
    const auto ndim = dims.size();
    H5::DataSpace aspace;
    if (ndim) {
        aspace = H5::DataSpace(sanisizer::cast<int>(ndim), dims.data());
    }

    auto atype = decode_data_type(raw_type);
    auto& ghandle = extract_group(gptr); 
    auto aptr = new H5::Attribute(ghandle.createAttribute(aname, atype, aspace));
    return Rcpp::XPtr<H5::Attribute>(aptr, true);
}
