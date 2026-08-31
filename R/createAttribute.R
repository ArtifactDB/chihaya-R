#' Create a HDF5 Attribute
#'
#' Create an Attribute on a HDF5 Group or DataSet.
#'
#' @param handle External pointer to a HDF5 Group or DataSet.
#' @param name String containing the name of the Attribute.
#' @param type List specifying the HDF5 type of the new Attribute,
#' see \code{\link{createStringType}} and related functions.
#' @param dims Numeric vector of dimension extents.
#' An empty vector or \code{NULL} indicates that the new Attribute is a scalar.
#'
#' @return External pointer to a HDF5 Attribute.
#'
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile(fileext = ".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#'
#' createAttribute(
#'     ghandle,
#'     "bar",
#'     type = createIntegerType(bits = 32, sign = TRUE),
#'     dims = NULL
#' )
#'
#' createAttribute(
#'     ghandle,
#'     "whee",
#'     type = createStringType(length = 10),
#'     dims = c(4, 3)
#' )
#'
#' listAttributes(ghandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
createAttribute <- function(handle, name, type, dims) {
    handle_type <- attr(handle, "type")
    ptr <- NULL
    if ("group" %in% handle_type) {
        ptr <- h5_create_group_attribute(handle, name, type, as.numeric(dims))
    } else if ("dataset" %in% handle_type) {
        ptr <- h5_create_group_attribute(handle, name, type, as.numeric(dims))
    } else {
        stop("unsupported handle type")
    }
    attr(ptr, "type") <- "attribute"
    ptr
}
