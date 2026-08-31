#' Write a HDF5 Attribute
#'
#' Write data to an existing HDF5 Attribute.
#'
#' @param handle External pointer to a HDF5 Attribute,
#' typically returned by \code{\link{openAttribute}} or \code{\link{createAttribute}}.
#' @param value Value of the HDF5 attribute as an atomic vector.
#'
#' @return \code{value} is written to \code{handle}, and \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#' ahandle <- createAttribute(
#'     ghandle,
#'     "bar",
#'     type = createFloatType(64),
#'     dim = NULL
#' )
#' writeAttribute(ahandle, 123.0)
#' closeAttribute(ahandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
writeAttribute <- function(handle, value) {
    handle_type <- attr(handle, "type")
    if ("group" %in% handle_type) {
        h5_write_group_attribute(handle, value)
    } else if ("dataset" %in% handle_type) {
        h5_write_dataset_attribute(handle, value)
    } else {
        stop("unsupported handle type")
    }
}
