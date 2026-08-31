#' List attributes
#'
#' List attributes on a HDF5 Group or DataSet.
#'
#' @param handle External pointer to a HDF5 Group or DataSet.
#'
#' @return Character vector of attribute names.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#'
#' createAttribute(
#'     ghandle,
#'     "bar",
#'     type=createIntegerType(bits=32, sign=TRUE)
#' )
#'
#' createAttribute(
#'     ghandle,
#'     "whee",
#'     type=createStringType(length=10)
#' )
#'
#' listAttributes(ghandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
listAttributes <- function(handle) {
    handle_type <- attr(handle, "type")
    if ("group" %in% handle_type) {
        h5_list_group_attributes(handle)
    } else if ("dataset" %in% handle_type) {
        h5_list_dataset_attributes(handle)
    } else {
        stop("unknown handle type")
    }
}
