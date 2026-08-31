#' Create a HDF5 Group 
#'
#' Create a new HDF5 Group.
#'
#' @param handle External pointer to a HDF5 Group. 
#' @param name String containing the name of the Group.
#'
#' @return External pointer to a HDF5 Group.
#'
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile(fileext = ".h5")
#' fhandle <- createFile(tmp)
#' ghandle <- createGroup(fhandle, "foo")
#' ghandle2 <- createGroup(ghandle, "bar")
#' closeGroup(ghandle2)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
createGroup <- function(handle, name) {
    stopifnot("group" %in% attr(handle, "type"))
    ptr <- h5_create_group(handle, name)
    attr(ptr, "type") <- "group"
    ptr
}
