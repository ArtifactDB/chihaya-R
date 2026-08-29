#include "Rcpp.h"
#include "H5Cpp.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_create_file(std::string path) {
    return Rcpp::XPtr<H5::Group>(new H5::H5File(path, H5F_ACC_TRUNC), true);
}
