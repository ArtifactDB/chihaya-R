#include "H5Cpp.h"
#include "Rcpp.h"
#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_open_group(Rcpp::RObject gptr, std::string gname) {
    auto& ghandle = extract_group(gptr);
    auto sub_gptr = new H5::Group(ghandle.openGroup(gname));
    return Rcpp::XPtr<H5::Group>(sub_gptr, true);
}
