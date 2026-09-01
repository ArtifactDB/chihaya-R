#' Check if a child object exists
#'
#' Check if a child object exists in a HDF5 Group. 
#'
#' @param handle External pointer to a HDF5 Group.
#' @param name String containing the name of the child object. 
#'
#' @return String specifying whether the named child object is a \code{"group"}, \code{"dataset"} or \code{"unknown"}.
#' If no child object exists at \code{name}, \code{"absent"} is returned instead.
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
