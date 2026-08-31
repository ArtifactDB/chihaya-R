#include "H5Cpp.h"
#include "Rcpp.h"
#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_open_dataset(Rcpp::RObject gptr, std::string dname) {
    auto& ghandle = extract_group(gptr);
    auto dptr = new H5::DataSet(ghandle.openDataSet(dname));
    return Rcpp::XPtr<H5::DataSet>(dptr, true);
}

//[[Rcpp::export(rng=false)]]
SEXP h5_close_dataset(Rcpp::RObject dptr) {
    Rcpp::XPtr<H5::DataSet> dxptr(dptr);
    dxptr.release();
    return R_NilValue;
}
