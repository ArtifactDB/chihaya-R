#' Check if a child object exists
#'
#' Check if a child object exists in a HDF5 Group. 
#'
#' @param handle External pointer to a HDF5 Group.
#' @param name String containing the name of the child object. 
#' @param report.type Boolean indicating whether to report the type of the child object, if it exists.
#'
#' @return
#' If \code{report.type = FALSE}, a boolean indicating whether the child exists in \code{handle}.
#'
#' If \code{report.type = TRUE}, a string specifying whether the named child object is a \code{"group"}, \code{"dataset"} or \code{"unknown"}.
#' If no child object exists at \code{name}, \code{NULL} is returned instead.
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
childExists <- function(handle, name, report.type = FALSE) {
    stopifnot("group" %in% attr(handle, "type"))
    h5_child_exists(handle, name, report.type)
}
