#ifndef UTILS_H
#define UTILS_H

#include "Rcpp.h"
#include "H5Cpp.h"

#include "sanisizer/sanisizer.hpp"

#include <type_traits>

inline H5::Group& extract_group(Rcpp::RObject ptr) {
    Rcpp::XPtr<H5::Group> gptr(ptr);
    return *gptr;
}

inline H5::DataSet& extract_dataset(Rcpp::RObject ptr) {
    Rcpp::XPtr<H5::DataSet> dptr(ptr);
    return *dptr;
}

inline H5::Attribute& extract_attribute(Rcpp::RObject ptr) {
    Rcpp::XPtr<H5::Attribute> aptr(ptr);
    return *aptr;
}

template<typename Input_>
using I = std::remove_cv_t<std::remove_reference_t<Input_> >;

#endif
