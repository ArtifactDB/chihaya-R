#include "H5Cpp.h"
#include "Rcpp.h"

#include "utils.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_create_group(Rcpp::RObject gptr, std::string name) {
    auto& ghandle = extract_group(gptr);
    auto ptr = new H5::Group(ghandle.createGroup(name));
    return Rcpp::XPtr<H5::Group>(ptr, true);
}
