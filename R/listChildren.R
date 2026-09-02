#' List child objects
#'
#' List child objects of a HDF5 Group.
#'
#' @param handle External pointer to a HDF5 Group.
#' 
#' @return List of two character vectors \code{group} and \code{dataset},
#' containing the names of the child Groups and DataSets of the HDF5 Group referenced by \code{handle}.
#'
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#' dhandle <- createDataSet(
#'     fhandle,
#'     "bar",
#'     type = createFloatType(32),
#'     dims = NULL
#' )
#'
#' listChildren(fhandle)
#' closeGroup(ghandle)
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#' 
#' @export
listChildren <- function(handle) {
    stopifnot("group" %in% attr(handle, "type"))
    h5_list_children(handle)
}
