#include "H5Cpp.h"
#include "Rcpp.h"

#include "utils.h"

#include <string>

//[[Rcpp::export(rng=false)]]
bool h5_group_attribute_exists(Rcpp::RObject gptr, std::string aname) {
    const auto& ghandle = extract_group(gptr);
    return ghandle.attrExists(aname);
}

//[[Rcpp::export(rng=false)]]
bool h5_dataset_attribute_exists(Rcpp::RObject dptr, std::string aname) {
    const auto& dhandle = extract_dataset(dptr);
    return dhandle.attrExists(aname);
}
