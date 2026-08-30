#ifndef EXTRACT_DIMENSIONS_H
#define EXTRACT_DIMENSIONS_H

#include "Rcpp.h"
#include "H5Cpp.h"

#include "sanisizer/sanisizer.hpp"

#include <vector>

inline std::vector<hsize_t> extract_dimensions(Rcpp::RObject dim) {
    std::vector<hsize_t> output;

    if (dim.sexp_type() == INTSXP) {
        Rcpp::IntegerVector idim(dim);
        output.reserve(idim.size());
        for (const auto id : idim) {
            if (id < 0) {
                throw std::runtime_error("expected non-negative integers for the dimensions");
            }
            output.push_back(sanisizer::cast<hsize_t>(id));
        }

    } else if (dim.sexp_type() == REALSXP) {
        Rcpp::NumericVector ddim(dim);
        output.reserve(ddim.size());
        for (const auto dd : ddim) {
            if (dd < 0) {
                throw std::runtime_error("expected non-negative integers for the dimensions");
            }
            output.push_back(sanisizer::from_float<hsize_t>(dd));
        }

    } else {
        throw std::runtime_error("expected a numeric vector for the dimensions");
    }

    return output;
}

#endif
