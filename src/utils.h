#include "Rcpp.h"
#include "H5Cpp.h"

#include "sanisizer/sanisizer.hpp"

inline hsize_t extract_hsize_t(Rcpp::RObject len) {
    if (len.sexp_type() == INTSXP) {
        Rcpp::IntegerVector ilen(len);
        if (ilen.size() != 1 || ilen[0] < 0) {
            throw std::runtime_error("expected a single non-negative integer as the length");
        }
        return sanisizer::cast<hsize_t>(ilen[0]);

    } else if (len.sexp_type() == REALSXP) {
        Rcpp::NumericVector dlen(len);
        if (dlen.size() != 1) {
            throw std::runtime_error("expected a single number as the length");
        }
        return sanisizer::from_float<hsize_t>(dlen[0]);

    } else {
        throw std::runtime_error("expected a number or integer as the length");
        return 0;
    }
}

inline H5::Group& extract_group(Rcpp::RObject ptr) {
    Rcpp::XPtr<H5::Group> gptr(ptr);
    return *gptr;
}

inline H5::DataSet& extract_dataset(Rcpp::RObject ptr) {
    Rcpp::XPtr<H5::DataSet> dptr(ptr);
    return *dptr;
}

inline H5::DataType extract_data_type(Rcpp::List info) {
    if (info.size() == 0) {
        throw std::runtime_error("expected type information list to have positive length");
    }

    const auto dclass = [&]{
        Rcpp::RObject raw_dclass(info[0]);
        if (raw_dclass.sexp_type() != STRSXP) {
            throw std::runtime_error("expected first entry of the type information list to be a string");
        }
        Rcpp::StringVector vec_dclass(raw_dclass);
        if (vec_dclass.size() != 1 || vec_dclass[0] == NA_STRING) {
            throw std::runtime_error("expected first entry of the type information list to be a non-missing string");
        }
        return Rcpp::as<std::string>(vec_dclass[0]);
    }();

    if (dclass == "string") {
        if (info.size() != 3) {
            throw std::runtime_error("expected string type information list to have length 3");
        }

        const auto strlen = [&]{
            Rcpp::RObject raw_strlen(info[1]);
            if (raw_strlen.sexp_type() != INTSXP) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            Rcpp::IntegerVector vec_strlen(raw_strlen);
            if (vec_strlen.size() != 1) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            return vec_strlen[0];
        }();

        const auto enc = [&]{
            Rcpp::RObject raw_enc(info[2]);
            if (raw_enc.sexp_type() != STRSXP) {
                throw std::runtime_error("expected third entry of the string type information list to be a string");
            }
            Rcpp::StringVector vec_enc(raw_enc);
            if (vec_enc.size() != 1 || vec_enc[0] == NA_STRING) {
                throw std::runtime_error("expected third entry of the string type information list to be a non-missing string");
            }
            const auto strenc = Rcpp::as<std::string>(vec_enc[0]);

            H5T_cset_t enc;
            if (strenc == "ASCII") {
                enc = H5T_CSET_ASCII;
            } else  if (strenc == "UTF-8") {
                enc = H5T_CSET_UTF8;
            } else {
                throw std::runtime_error("unknown encoding for string types");
            }
            return enc;
        }();

        if (strlen > 0) {
            H5::StrType output(0, strlen);
            output.setCset(enc);
            return output;
        } else {
            H5::StrType output(0, H5T_VARIABLE);
            output.setCset(enc);
            return output;
        }
        
    } else if (dclass == "integer") {
        if (info.size() != 3) {
            throw std::runtime_error("expected integer type information list to have length 3");
        }

        const auto bits = [&]{
            Rcpp::RObject raw_bits(info[1]);
            if (raw_bits.sexp_type() != INTSXP) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            Rcpp::IntegerVector vec_bits(raw_bits);
            if (vec_bits.size() != 1) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            return vec_bits[0];
        }();

        const auto sign = [&]{
            Rcpp::RObject raw_signs(info[2]);
            if (raw_signs.sexp_type() != LGLSXP) {
                throw std::runtime_error("expected third entry of the string type information list to be a boolean");
            }
            Rcpp::LogicalVector vec_signs(raw_signs);
            if (vec_signs.size() != 1) {
                throw std::runtime_error("expected second entry of the string type information list to be a boolean");
            }
            return vec_signs[0];
        }();

        if (sign) {
            if (bits == 8) {
                return H5::PredType::NATIVE_INT8;
            } else if (bits == 16) {
                return H5::PredType::NATIVE_INT16;
            } else if (bits == 32) {
                return H5::PredType::NATIVE_INT32;
            } else if (bits == 64) {
                return H5::PredType::NATIVE_INT64;
            }
        } else {
            if (bits == 8) {
                return H5::PredType::NATIVE_UINT8;
            } else if (bits == 16) {
                return H5::PredType::NATIVE_UINT16;
            } else if (bits == 32) {
                return H5::PredType::NATIVE_UINT32;
            } else if (bits == 64) {
                return H5::PredType::NATIVE_UINT64;
            }
        }

    } else if (dclass == "float") {
        if (info.size() != 2) {
            throw std::runtime_error("expected floating-point type information list to have length 2");
        }

        const auto bits = [&]{
            Rcpp::RObject raw_bits(info[1]);
            if (raw_bits.sexp_type() != INTSXP) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            Rcpp::IntegerVector vec_bits(raw_bits);
            if (vec_bits.size() != 1) {
                throw std::runtime_error("expected second entry of the string type information list to be an integer");
            }
            return vec_bits[0];
        }();

        if (bits == 32) {
            return H5::PredType::NATIVE_FLOAT;
        } else if (bits == 64) {
            return H5::PredType::NATIVE_DOUBLE;
        }
    }

    throw std::runtime_error("unknown type for the specified parameters");
    return H5::PredType::NATIVE_DOUBLE; // just provided to avoid compiler warnings and is otherwise ignored.
}
