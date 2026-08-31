#' Write a HDF5 DataSet 
#'
#' Write data to an existing HDF5 DataSet.
#'
#' @param handle External pointer to a HDF5 DataSet,
#' typically returned by \code{\link{openDataSet}} or \code{\link{createDataSet}}.
#' @param value Value of the HDF5 attribute as an atomic vector.
#'
#' @return \code{value} is written to \code{handle}, and \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' fhandle <- createFile(tmp)
#' dhandle <- createDataSet(
#'     fhandle,
#'     "bar",
#'     type = createFloatType(64),
#'     dim = c(10, 20)
#' )
#' writeDataSet(dhandle, 1:200)
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#'
#' @export
writeDataSet <- function(handle, value) {
    stopifnot("dataset" %in% attr(handle, "type"))
    h5_write_dataset(handle, value)
}
