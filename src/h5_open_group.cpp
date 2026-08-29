#include "H5Cpp.h"
#include "Rcpp.h"
#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_open_group(Rcpp::RObject handle, std::string name) {
    auto& ghandle = extract_group(handle);
    auto ptr = new H5::Group(ghandle.openGroup(name));
    return Rcpp::XPtr<H5::Group>(ptr, true);
}
