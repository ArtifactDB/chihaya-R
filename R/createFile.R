#' Create a HDF5 file
#'
#' Create a new HDF5 file.
#'
#' @param path String containing the path to a new HDF5 file.
#' If any existing file is present, it will be overwritten.
#'
#' @return An external pointer to a writeable HDF5 group representing the new file.
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile(fileext=".h5")
#' handle <- createFile(tmp)
#' closeGroup(handle)
#' file.info(tmp)
#'
#' @export
createFile <- function(path) {
    ptr <- h5_create_file(path)
    attr(ptr, "type") <- "group"
    ptr
}
