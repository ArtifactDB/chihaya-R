#' Check if an Attribute exists
#'
#' Check if an Attribute exists on a HDF5 Group or DataSet.
#'
#' @param handle External pointer to a HDF5 Group or DataSet.
#' @param name String containing the name of the Attribute. 
#'
#' @return Boolean indicating whether the named Attribute exists.
#'
#' @author Aaron Lun
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
#'     type = createStringType(length = 10)
#'     dims = NULL
#' )
#'
#' attributeExists(ghandle, "bar")
#' attributeExists(ghandle, "whee")
#' attributeExists(ghandle, "stuff")
#'
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
attributeExists <- function(handle, name) {
    handle_type <- attr(handle, "type")
    if ("group" %in% handle_type) {
        h5_group_attribute_exists(handle, name)
    } else if ("dataset" %in% handle_type) {
        h5_dataset_attribute_exists(handle, name)
    } else {
        stop("unknown handle type")
    }
}
