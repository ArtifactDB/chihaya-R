#' Open and close a HDF5 Group 
#'
#' Open and close an existing HDF5 Group.
#'
#' @param handle External pointer to a HDF5 Group.
#' This can returned by \code{openGroup} or other functions like \code{\link{openFile}} or \code{\link{createFile}}.
#' @param name String containing the name of the child Group to open.
#'
#' @return
#' For \code{openGroup}, an external pointer to the HDF5 group. 
#'
#' For \code{closeGroup}, the Group referenced by \code{handle} is closed.
#' \code{handle} itself should no longer be used.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' It is not strictly necessary to call \code{closeGroup} as the Group will be automatically closed upon garbage collection of the relevant pointers.
#' Nonetheless, it can be helpful to explicitly close the group to release system resources (e.g., memory, file handles) in a predictable manner.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' handle <- createFile(tmp)
#' createGroup(handle, "foo")
#' closeFile(handle)
#'
#' new_handle <- openFile(handle)
#' ghandle <- openGroup(new_handle, "foo")
#' closeGroup(ghandle)
#' closeGroup(new_handle)
#'
#' @export
openGroup <- function(handle, name) {
    stopifnot("group" %in% attr(handle, "type"))
    ptr <- h5_open_group(handle, name)
    attr(ptr, "type") <- "group"
    ptr
}

#' @export
closeGroup <- function(handle) {
    stopifnot("group" %in% attr(handle, "type"))
    h5_close_group(handle)
    invisible(NULL)
}
