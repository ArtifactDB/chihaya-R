#include "H5Cpp.h"
#include "Rcpp.h"
#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_open_group_attribute(Rcpp::RObject gptr, std::string aname) {
    auto& ghandle = extract_group(gptr);
    auto aptr = new H5::Attribute(ghandle.openAttribute(aname));
    return Rcpp::XPtr<H5::Attribute>(aptr, true);
}

//[[Rcpp::export(rng=false)]]
SEXP h5_open_dataset_attribute(Rcpp::RObject dptr, std::string aname) {
    auto& dhandle = extract_dataset(dptr);
    auto aptr = new H5::Attribute(dhandle.openAttribute(aname));
    return Rcpp::XPtr<H5::Attribute>(aptr, true);
}

//[[Rcpp::export(rng=false)]]
SEXP h5_close_attribute(Rcpp::RObject aptr) {
    Rcpp::XPtr<H5::Attribute> axptr(aptr);
    axptr.release();
    return R_NilValue;
}
