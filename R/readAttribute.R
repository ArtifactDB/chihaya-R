#' Read a HDF5 Attribute
#'
#' Read the contents of a HDF5 Attribute.
#'
#' @param handle External pointer to a HDF5 Attribute, typically returned by \code{\link{openAttribute}}.
#'
#' @return List containing:
#' \itemize{
#' \item \code{value}, the contents of the Attribute as an atomic vector.
#' \item \code{dims}, numeric vector containing the dimensions of the Attribute.
#' }
#'
#' @details
#' Integer data may be promoted to double-precision \code{value},
#' if the range of values of the integer type is not a subset of the range of values of a signed 32-bit integer.
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
#' closeAttribute(ahandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' fhandle <- openFile(handle)
#' ghandle <- openGroup(fhandle, "foo")
#' ahandle <- openAttribute(ghandle, "bar")
#' readAttribute(ahandle)
#' closeAttribute(ahandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
readAttribute <- function(handle) {
    stopifnot("attribute" %in% attr(handle, "type"))
    h5_read_attribute(handle)
}
