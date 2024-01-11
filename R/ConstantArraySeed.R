#' Saving a ConstantArraySeed
#'
#' Save a \linkS4class{ConstantArraySeed} object.
#' See the \dQuote{Constant array} section at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{ConstantArraySeed} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the ConstantArraySeed.
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- ConstantArray(value=NA_real_, dim=c(11, 25))
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(X, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname ConstantArraySeed 
#' @import rhdf5 alabaster.base
setMethod("saveDelayedObject", "ConstantArraySeed", function(x, file, name, version=package_version("1.1"), ...) {
    fhandle <- H5Fopen(file, "H5F_ACC_RDWR")
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
    ghandle <- H5Gcreate(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "array", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_array", "constant array", scalar=TRUE)
    h5_write_vector(ghandle, "dimensions", dim(x), compress=0, type="H5T_NATIVE_UINT32")
    save_vector(ghandle, "value", x@value, version=version, scalar=TRUE)

    invisible(NULL)
})

#' @import DelayedArray
.load_constant_array <- function(file, name, contents, version) {
    fhandle <- H5Fopen(file, "H5F_ACC_RDONLY")
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    dim <- h5_read_vector(ghandle, "dimensions")
    val <- load_vector(ghandle, "value", drop=TRUE, version=version)

    ConstantArray(dim, value=val)
}
