#' @import alabaster.base rhdf5
load_vector <- function(handle, name, drop, version) {
    dhandle <- H5Dopen(handle, name)
    on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
    contents <- H5Dread(dhandle, drop=drop) 
    if (is.raw(contents)) {
        storage.mode(contents) <- "integer"
    }

    type <- NULL
    if (version >= package_version("1.1")) {
        type <- h5_read_attribute(dhandle, "type")
        type <- to_alabaster_type(type)
    }

    if (version >= package_version("1.0")) {
        missing.placeholder <- h5_read_attribute(dhandle, "missing_placeholder", check=TRUE, default=NULL)
    } else {
        # Back-compatibility for the old undocumented way of holding missing values.
        missing.placeholder <- h5_read_attribute(dhandle, "missing-value-placeholder", check=TRUE, default=NULL)
    }

    h5_cast(contents, expected.type=type, missing.placeholder=missing.placeholder, respect.nan.payload=(version < package_version("1.1")))
}

#' @import alabaster.base rhdf5
save_vector <- function(handle, name, x, version, scalar) {
    info <- transformVectorForHdf5(x)
    dhandle <- h5_write_vector(handle, name, info$transformed, scalar=scalar, emit=TRUE)
    on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)

    if (version >= package_version("1.0")) {
        if (!is.null(info$placeholder)) {
            h5_write_attribute(dhandle, "missing_placeholder", info$placeholder, scalar=TRUE)
        }
    }
    if (version >= package_version("1.1")) {
        h5_write_attribute(dhandle, "type", to_value_type(type(x)), scalar=TRUE)
    }

    invisible(NULL)
}

r2value_type_mapping <- c(logical="BOOLEAN", integer="INTEGER", double="FLOAT", character="STRING")
to_value_type <- function(type) {
    if (!(type %in% names(r2value_type_mapping))) {
        stop("cannot map type '", type, "' to a value type")
    }
    r2value_type_mapping[[type]]
}

value2alabaster_type_mapping <- c(BOOLEAN="boolean", INTEGER="integer", FLOAT="number", STRING="string")
to_alabaster_type <- function(vtype) {
    if (!(vtype %in% names(value2alabaster_type_mapping))) {
        stop("cannot map type '", type, "' to an alabaster type")
    }
    value2alabaster_type_mapping[[vtype]]

}


#' @export
#' @rdname chihaya-utils
.saveList <- function(file, name, x, parent=NULL, vectors.only=FALSE) { 
    if (!is.null(parent)) {
        name <- file.path(parent, name)
    }

    h5createGroup(file, name)
    .label_group(file, name, list(delayed_type = "list", delayed_length=length(x)))

    for (i in seq_along(x)) {
        if (!is.null(x[[i]])) {
            j <- i - 1L
            if (vectors.only) {
                h5write(x[[i]], file, file.path(name, j))
            } else {
                saveDelayedObject(x[[i]], file, file.path(name, j))
            }
        }
    }

    invisible(NULL)
}

#' @export
#' @rdname chihaya-utils
#' @importFrom rhdf5 h5read
.loadList <- function(file, name, parent=NULL, vectors.only=FALSE) {
    if (!is.null(parent)) {
        name <- file.path(parent, name)
    }

    attrs <- h5readAttributes(file, name)
    vals <- vector("list", attrs$delayed_length)

    for (i in seq_along(vals)) {
        j <- as.character(i - 1L)
        if (!h5exists(file, name, j)) {
            next
        }

        if (vectors.only) {
            vals[[i]] <- h5read(file, file.path(name, j), drop=TRUE)
        } else {
            vals[[i]] <- .dispatch_loader(file, file.path(name, j))
        }
    }
    names(vals) <- as.vector(attrs$delayed_names)
    vals
}
