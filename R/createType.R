#' Specify a HDF5 data type
#'
#' Create a specification for a HDF5 data type,
#' to be used in functions like \code{\link{createDataSet}} and \code{\link{createAttribute}}.
#'
#' @param length Positive integer specifying the maximum length of a fixed-length string.
#' Alternatively zero or \code{NULL}, in which case a variable-length string type is created.
#' @param encoding String specifying the encoding of the string.
#' @param bits Integer specifying the number of bits in the integer or floating-point number.
#' For \code{createIntegerType}, this should be one of 8, 16, 32 or 64.
#' For \code{createFloatType}, this should be one of 32 or 64.
#' @param sign Boolean specifying whether the integer is signed.
#'
#' @return List containing information about the HDF5 data type.
#'
#' @author Aaron Lun
#' @examples
#' createStringType(10)
#' createIntegerType(8, TRUE)
#' createFloatType(64)
#' 
#' @name createType
#' @export
createStringType <- function(length = NULL, encoding = c("UTF-8", "ASCII")) {
    if (is.null(length)) {
        length <- 0L
    }
    list(type = "string", length = as.integer(length), encoding = match.arg(encoding))
}

#' @export
#' @rdname createType
createIntegerType <- function(bits = 32L, sign = TRUE) {
    stopifnot(bits %in% c(8L, 16L, 32L, 64L))
    list(type = "integer", bits = as.integer(bits), sign = as.logical(sign))
}

#' @export
#' @rdname createType
createFloatType <- function(bits = 64L) {
    stopifnot(bits %in% c(32L, 64L))
    list(type = "float", bits = as.integer(bits))
}
