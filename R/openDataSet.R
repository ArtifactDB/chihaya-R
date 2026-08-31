#' Open and close a HDF5 DataSet.
#'
#' Open and close an existing HDF5 DataSet.
#'
#' @param handle 
#' For \code{openDataSet}, an external pointer to a HDF5 Group. 
#' This can returned by \code{\link{openGroup}} or other functions like \code{\link{createGroup}}.
#'
#' For \code{closeDataSet}, an external pointer to a HDF5 DataSet.
#' This can returned by \code{\link{openDataSet}} or other functions like \code{\link{createDataSet}}.
#' @param name String containing the name of the child DataSet to open.
#'
#' @return
#' For \code{openDataSet}, an external pointer to the HDF5 DataSet. 
#'
#' For \code{closeDataSet}, the DataSet referenced by \code{handle} is closed.
#' \code{handle} itself should no longer be used.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' It is not strictly necessary to call \code{closeDataSet} as the DataSet will be automatically closed upon garbage collection of the relevant pointers.
#' Nonetheless, it can be helpful to explicitly close the group to release system resources (e.g., memory, file handles) in a predictable manner.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' dhandle <- createDataSet(
#'     fhandle,
#'     "bar",
#'     type = createFloatType(32),
#'     dim = c(10, 20) 
#' )
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#'
#' fhandle <- openFile(tmp)
#' dhandle <- openDataSet(fhandle, "bar")
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#'
#' @export
openDataSet <- function(handle, name) {
    stopifnot("group" %in% attr(handle, "type"))
    ptr <- h5_open_dataset(handle, name)
    attr(ptr, "type") <- "dataset"
    ptr
}

#' @export
closeDataSet <- function(handle) {
    stopifnot("dataset" %in% attr(handle, "type"))
    h5_close_dataset(handle)
    invisible(NULL)
}
