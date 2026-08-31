#' Open and close a HDF5 Attribute
#'
#' Open and close an existing HDF5 Attribute.
#'
#' @param handle 
#' For \code{openAttribute}, an external pointer to a HDF5 Group or DataSet.
#' This can returned by \code{\link{openGroup}} or other functions like \code{\link{createDataSet}}.
#'
#' For \code{closeAttribute}, an external pointer to a HDF5 Attribute.
#' This can returned by \code{\link{openAttribute}} or other functions like \code{\link{createAttribute}}.
#' @param name String containing the name of the child Attribute to open.
#'
#' @return
#' For \code{openAttribute}, an external pointer to the HDF5 Attribute. 
#'
#' For \code{closeGroup}, the Attribute referenced by \code{handle} is closed.
#' \code{handle} itself should no longer be used.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' It is not strictly necessary to call \code{closeAttribute} as the Attribute will be automatically closed upon garbage collection of the relevant pointers.
#' Nonetheless, it can be helpful to explicitly close the group to release system resources (e.g., memory, file handles) in a predictable manner.
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
#' closeAttribute(ahandle)
#' closeGroup(ghandle)
#' closeGroup(fhandle)
#'
#' @export
openAttribute <- function(handle, name) {
    handle_type <- attr(handle, "type")
    ptr <- NULL
    if ("group" %in% handle_type) {
        ptr <- h5_open_group_attribute(handle, name)
    } else if ("dataset" %in% handle_type) {
        ptr <- h5_open_dataset_attribute(handle, name)
    } else {
        stop("unsupported handle type")
    }
    attr(ptr, "type") <- "attribute"
    ptr
}

#' @export
closeAttribute <- function(handle) {
    stopifnot("attribute" %in% attr(handle, "type"))
    h5_close_attribute(handle)
    invisible(NULL)
}
