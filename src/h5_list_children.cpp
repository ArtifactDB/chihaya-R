#include "H5Cpp.h"
#include "Rcpp.h"

#include "utils.h"

#include "sanisizer/sanisizer.hpp"

#include <vector>
#include <string>

//[[Rcpp::export(rng=false)]]
Rcpp::List h5_list_children(Rcpp::RObject gptr) {
    const auto& ghandle = extract_group(gptr);
    const auto nchildren = ghandle.getNumObjs();

    std::vector<std::string> child_datasets, child_groups;
    for (I<decltype(nchildren)> i = 0; i < nchildren; ++i) {
        auto childname = ghandle.getObjnameByIdx(i);
        auto childtype = ghandle.childObjType(childname);
        if (childtype == H5O_TYPE_GROUP) {
            child_groups.push_back(std::move(childname));
        } else if (childtype == H5O_TYPE_DATASET) {
            child_datasets.push_back(std::move(childname));
        } else {
            // skipping other things that we don't understand.
        }
    }

    const auto num_datasets = child_datasets.size();
    auto dataset_names = sanisizer::create<Rcpp::StringVector>(num_datasets);
    for (I<decltype(num_datasets)> d = 0; d < num_datasets; ++d) {
        dataset_names[d] = child_datasets[d];
    }

    const auto num_groups = child_groups.size();
    auto group_names = sanisizer::create<Rcpp::StringVector>(num_groups);
    for (I<decltype(num_groups)> d = 0; d < num_groups; ++d) {
        group_names[d] = child_groups[d];
    }

    return Rcpp::List::create(
        Rcpp::Named("dataset") = dataset_names,
        Rcpp::Named("group") = group_names
    );
}
