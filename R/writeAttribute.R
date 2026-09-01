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
    stopifnot("attribute" %in% attr(handle, "type"))
    h5_write_attribute(handle, value)
}
