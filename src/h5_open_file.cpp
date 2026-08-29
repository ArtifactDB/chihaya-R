#include "Rcpp.h"
#include "H5Cpp.h"

//[[Rcpp::export(rng=false)]]
SEXP h5_open_file(std::string path, bool read_only) {
    return Rcpp::XPtr<H5::Group>(new H5::H5File(path, (read_only ? H5F_ACC_RDONLY : H5F_ACC_RDWR)), true);
}
