#' Read a HDF5 DataSet 
#'
#' Read the contents of a HDF5 DataSet.
#'
#' @param handle External pointer to a HDF5 DataSet, typically returned by \code{\link{openDataSet}}.
#'
#' @return List containing:
#' \itemize{
#' \item \code{value}, the contents of the DataSet as an atomic vector.
#' \item \code{dims}, numeric vector containing the dimensions of the DataSet.
#' }
#'
#' @details
#' Integer data may be promoted to double-precision \code{value},
#' if the range of values of the integer type is not a subset of the range of values of a signed 32-bit integer.
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
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#'
#' fhandle <- openFile(handle)
#' dhandle <- openDataSet(fhandle, "bar")
#' readDataSet(dhandle)
#' closeDataSet(dhandle)
#' closeGroup(fhandle)
#'
#' @export
readDataSet <- function(handle) {
    stopifnot("dataset" %in% attr(handle, "type"))
    h5_read_dataset(handle)
}
