#include "H5Cpp.h"
#include "Rcpp.h"

#include "utils.h"

#include <string>

//[[Rcpp::export(rng=false)]]
SEXP h5_child_exists(Rcpp::RObject gptr, std::string name) {
    const auto& ghandle = extract_group(gptr);
    if (!ghandle.nameExists(name)) {
        return R_NilValue;
    }

    auto childtype = ghandle.childObjType(name);
    Rcpp::StringVector output(1);
    if (childtype == H5O_TYPE_GROUP) {
        output[0] = "group";
    } else if (childtype == H5O_TYPE_DATASET) {
        output[0] = "dataset";
    } else {
        output[0] = "unknown";
    }

    return output;
}
