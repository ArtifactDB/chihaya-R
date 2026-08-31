#' Check if a child object exists
#'
#' Check if a child object exists in a HDF5 Group. 
#'
#' @param handle External pointer to a HDF5 Group.
#' @param name String containing the name of the child object. 
#'
#' @return Boolean indicating whether the named child object exists.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#'
#' attributeExists(fhandle, "foo")
#' attributeExists(fhandle, "bar")
#'
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
childExists <- function(handle, name) {
    stopifnot("group" %in% attr(handle, "type"))
    h5_child_exists(handle, name)
}
