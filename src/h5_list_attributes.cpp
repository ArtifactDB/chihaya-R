#include "H5Cpp.h"
#include "Rcpp.h"

#include "utils.h"

#include "sanisizer/sanisizer.hpp"

#include <vector>
#include <string>

//[[Rcpp::export(rng=false)]]
Rcpp::StringVector h5_list_group_attributes(Rcpp::RObject gptr) {
    const auto& ghandle = extract_group(gptr);
    const auto nattrs = ghandle.getNumAttrs();

    auto output = sanisizer::create<Rcpp::StringVector>(nattrs);
    for (I<decltype(nattrs)> i = 0; i < nattrs; ++i) {
        auto attr = ghandle.openAttribute(i);
        output[i] = attr.getName();
    }

    return output;
}

//[[Rcpp::export(rng=false)]]
Rcpp::StringVector h5_list_dataset_attributes(Rcpp::RObject dptr) {
    const auto& dhandle = extract_dataset(dptr);
    const auto nattrs = dhandle.getNumAttrs();

    auto output = sanisizer::create<Rcpp::StringVector>(nattrs);
    for (I<decltype(nattrs)> i = 0; i < nattrs; ++i) {
        auto attr = dhandle.openAttribute(i);
        output[i] = attr.getName();
    }

    return output;
}
