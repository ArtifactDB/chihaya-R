#' Create a HDF5 DataSet
#'
#' Create a new DataSet in a HDF5 Group. 
#'
#' @param handle External pointer to a HDF5 Group. 
#' @param name String containing the name of the DataSet.
#' @param type List specifying the HDF5 type of the new DataSet,
#' see \code{\link{createStringType}} and related functions.
#' @param dims Numeric vector of dimension extents.
#' An empty vector or \code{NULL} indicates that the new attribute is a scalar.
#' @param compress Integer specifying the DEFLATE compression level.
#' Larger values increase compression efficiency at the cost of extra compute.
#' If set to zero, no compression is performed.
#' @param chunkdims Numeric vector of chunk dimension extents.
#' This should be of the same length as \code{dims}.
#' Ignored if \code{compress = 0} or any entry of \code{dims} is zero.
#'
#' @return External pointer to a HDF5 DataSet.
#'
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile(fileext = ".h5")
#' fhandle <- createFile(tmp)
#' createDataSet(
#'     ghandle,
#'     "bar",
#'     type = createIntegerType(bits = 32, sign = TRUE),
#'     dims = c(10, 20, 5) 
#' )
#' closeGroup(fhandle)
#'
#' @export
createDataSet <- function(handle, name, type, dims, compress = 6, chunkdims = NULL) {
    stopifnot("group" %in% attr(handle, "type"))

    if (is.null(chunkdims)) {
        chunkdims <- ceiling(dims / prod(dims) * 10000)
    }

    ptr <- h5_create_dataset(
        handle,
        name,
        raw_type = type,
        raw_dim = as.numeric(dims),
        compress = compress,
        raw_chunks = as.numeric(chunkdims),
        is_vlen_str = (type[[1]] == "string" && type[[2]] == 0L)
    )

    attr(ptr, "type") <- "dataset"
    ptr
}
