#' Open a HDF5 file
#'
#' Open an existing HDF5 file.
#'
#' @param path String containing the path to an existing HDF5 file.
#' @param read.only Boolean indicating whether to open the file in read-only mode.
#' If \code{FALSE}, the file is opened in read-write mode.
#'
#' @return External pointer to a HDF5 group representing the file contents. 
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' handle <- createFile(tmp)
#' closeGroup(handle)
#'
#' new_handle <- openFile(tmp)
#' listChildren(new_handle)
#' closeGroup(new_handle)
#'
#' @export
openFile <- function(path, read.only = TRUE) {
    ptr <- h5_open_file(path, read_only = read.only)
    attr(ptr, "type") <- "group"
    ptr
}
