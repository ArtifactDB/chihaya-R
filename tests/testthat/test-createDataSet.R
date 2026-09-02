# library(testthat); library(chihaya); source("test-createDataSet.R")

configs <- list(
    # Scalar.
    list(compress = TRUE, dims = integer(0)),

    # Small chunks.
    list(compress = TRUE, dims = 121L, chunkdims = 15L),
    list(compress = TRUE, dims = c(103L, 221L), chunkdims = c(10L, 15L)), 
    list(compress = TRUE, dims = c(77L, 21L, 34L), chunkdims = c(8L, 12L, 9L)),

    # The entire dataset is one chunk (capped at the dimension sizes).
    list(compress = TRUE, dims = 121L, chunkdims = 1000),
    list(compress = TRUE, dims = c(103L, 221L), chunkdims = c(1000, 1000)), 
    list(compress = TRUE, dims = c(77L, 21L, 34L), chunkdims = c(1000, 1000, 1000)),

    # The entire dataset is one slice.
    list(compress = FALSE, dims = 107L),
    list(compress = FALSE, dims = c(198L, 87L)), 
    list(compress = FALSE, dims = c(23L, 35L, 49L))
)

test_that("datasets work correctly for small integers", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    for (cf in seq_along(configs)) {
        dims <- configs[[cf]]$dims
        chunkdims <- configs[[cf]]$chunkdims
        compress <- configs[[cf]]$compress
        if (length(dims) == 0L) {
            payload <- 99L
        } else {
            payload <- sample(100L, prod(dims), replace=TRUE)
        }

        for (type in list(
            createIntegerType(8, FALSE),
            createIntegerType(8, TRUE),
            createIntegerType(16, FALSE),
            createIntegerType(16, TRUE),
            createIntegerType(32, TRUE)
        )) {
            dname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", cf)
            dhandle <- createDataSet(
                fhandle,
                dname,
                type = type,
                compress = if (compress) 6 else 0,
                dims = dims,
                chunk.dims = chunkdims
            )
            writeDataSet(dhandle, payload)

            reloaded <- readDataSet(dhandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeDataSet(dhandle)

            # Same result if we re-open it.
            dhandle2 <- openDataSet(fhandle, dname)
            reloaded2 <- readDataSet(dhandle2)
            expect_identical(reloaded, reloaded2)
            closeDataSet(dhandle2)
        }
    }

    closeGroup(fhandle)
})

test_that("datasets work correctly for large integers", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    for (cf in seq_along(configs)) {
        dims <- configs[[cf]]$dims
        chunkdims <- configs[[cf]]$chunkdims
        compress <- configs[[cf]]$compress
        if (length(dims) == 0L) {
            payload <- 999999L
        } else {
            payload <- sample(1000L, prod(dims), replace=TRUE)
        }

        for (type in list(
            createIntegerType(32, FALSE),
            createIntegerType(64, TRUE),
            createIntegerType(64, FALSE)
        )) {
            dname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", cf)
            dhandle <- createDataSet(
                fhandle,
                dname,
                type = type,
                compress = if (compress) 6 else 0,
                dims = dims,
                chunk.dims = chunkdims
            )
            writeDataSet(dhandle, payload)

            reloaded <- readDataSet(dhandle)
            expect_identical(reloaded$value, as.numeric(payload))
            expect_identical(reloaded$dims, dims)
            closeDataSet(dhandle)

            # Same result if we re-open it.
            dhandle2 <- openDataSet(fhandle, dname)
            reloaded2 <- readDataSet(dhandle2)
            expect_identical(reloaded, reloaded2)
            closeDataSet(dhandle2)
        }
    }

    closeGroup(fhandle)
})

test_that("datasets work correctly for floats", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    for (cf in seq_along(configs)) {
        dims <- configs[[cf]]$dims
        chunkdims <- configs[[cf]]$chunkdims
        compress <- configs[[cf]]$compress
        if (length(dims) == 0L) {
            payload <- -0.5 
        } else {
            n <- prod(dims)
            payload <- 2^sample(-10:10, n, replace=TRUE) * sample(c(-1, 1), n, replace=TRUE)
        }

        for (bits in c(32, 64)) {
            dname <- paste0("foo_F", bits, "_", cf)
            dhandle <- createDataSet(
                fhandle,
                dname,
                type = createFloatType(bits),
                compress = if (compress) 6 else 0,
                dims = dims,
                chunk.dims = chunkdims
            )
            writeDataSet(dhandle, payload)

            reloaded <- readDataSet(dhandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeDataSet(dhandle)

            # Same result if we re-open it.
            dhandle2 <- openDataSet(fhandle, dname)
            reloaded2 <- readDataSet(dhandle2)
            expect_identical(reloaded, reloaded2)
            closeDataSet(dhandle2)
        }
    }

    closeGroup(fhandle)
})

test_that("datasets work correctly for strings", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    for (cf in seq_along(configs)) {
        dims <- configs[[cf]]$dims
        chunkdims <- configs[[cf]]$chunkdims
        compress <- configs[[cf]]$compress
        if (length(dims) == 0L) {
            payload <- "FOOBAR"
        } else {
            n <- prod(dims)
            payload <- paste0("FOO_", seq_len(n))
        }

        for (strlen in c(0, 10)) {
            dname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen), "_", cf)
            dhandle <- createDataSet(
                fhandle,
                dname,
                type = createStringType(strlen, "UTF-8"),
                dims = dims,
                chunk.dims = chunkdims,
                compress = if (compress) 6 else 0
            )
            writeDataSet(dhandle, payload)

            reloaded <- readDataSet(dhandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeDataSet(dhandle)

            # Same result if we re-open it.
            dhandle2 <- openDataSet(fhandle, dname)
            reloaded2 <- readDataSet(dhandle2)
            expect_identical(reloaded, reloaded2)
            closeDataSet(dhandle2)
        }
    }

    closeGroup(fhandle)
})

test_that("datasets work with zero extents", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    # This tests a few things that should happen with zero extents:
    # - compression is turned off in writeDataSet.
    # - chunk-wise array traversal is skipped in writeDataSet and readDataSet.

    dname <- "iBAR"
    dhandle <- createDataSet(fhandle, dname, type = createIntegerType(), dims = c(0, 0))
    writeDataSet(dhandle, integer(0))
    expect_identical(readDataSet(dhandle)$value, integer(0))
    closeDataSet(dhandle)

    dname <- "iBAR2"
    dhandle <- createDataSet(fhandle, dname, type = createIntegerType(64, TRUE), dims = c(0, 10))
    writeDataSet(dhandle, integer(0))
    expect_identical(readDataSet(dhandle)$value, numeric(0))
    closeDataSet(dhandle)

    dname <- "fBAR"
    dhandle <- createDataSet(fhandle, dname, type = createFloatType(64), dims = c(20, 0))
    writeDataSet(dhandle, numeric(0))
    expect_identical(readDataSet(dhandle)$value, numeric(0))
    closeDataSet(dhandle)

    dname <- "sBAR"
    dhandle <- createDataSet(fhandle, dname, type = createStringType(), dims = 0)
    writeDataSet(dhandle, character(0))
    expect_identical(readDataSet(dhandle)$value, character(0))
    closeDataSet(dhandle)

    dname <- "sBAR2"
    dhandle <- createDataSet(fhandle, dname, type = createStringType(10), dims = 0)
    writeDataSet(dhandle, character(0))
    expect_identical(readDataSet(dhandle)$value, character(0))
    closeDataSet(dhandle)

    closeGroup(fhandle)
})

test_that("writeDataSet checks the dimension extents", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    dhandle <- createDataSet(fhandle, "foo", type = createIntegerType(), dims = 10)
    expect_error(writeDataSet(dhandle, 1:5), "not consistent")
    closeDataSet(dhandle)

    dhandle <- createDataSet(fhandle, "bar", type = createFloatType(), dims = c(5, 2))
    expect_error(writeDataSet(dhandle, runif(6)), "not consistent")
    closeDataSet(dhandle)

    dhandle <- createDataSet(fhandle, "stuff", type = createIntegerType(8, FALSE), dims = c(3, 2, 1))
    expect_error(writeDataSet(dhandle, c(TRUE, FALSE)), "not consistent")
    closeDataSet(dhandle)

    dhandle <- createDataSet(fhandle, "whee", type = createStringType(10), dims = c(3, 9))
    expect_error(writeDataSet(dhandle, "FOOBAR"), "not consistent")
    expect_error(writeDataSet(dhandle, c(LETTERS, NA)), "missing strings")
    closeDataSet(dhandle)

    closeGroup(fhandle)
})

test_that("createDataSet checks the chunk extents", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    dname <- "FOO"
    expect_error(createDataSet(fhandle, dname, type = createIntegerType(16, TRUE), dims = c(20, 5), chunk.dims = 50), "same length")

    dname <- "FOO2"
    expect_error(createDataSet(fhandle, dname, type = createIntegerType(16, TRUE), dims = c(20, 5), chunk.dims = c(-1L, -1L)), "non-negative")

    dname <- "FOO3"
    expect_error(createDataSet(fhandle, dname, type = createIntegerType(16, TRUE), dims = c(20, 5), chunk.dims = c(-1, -1)), "non-negative")
    closeGroup(fhandle)
})

test_that("writeDataSet truncates fixed-length strings", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    payload <- c("akira", "ai", "akari", "alice", "athena", "alicia", "aika")
    dhandle <- createDataSet(fhandle, "foo", type = createStringType(3), dims = length(payload))
    writeDataSet(dhandle, payload)
    closeDataSet(dhandle)

    dhandle <- createDataSet(fhandle, "bar", type = createStringType(NULL), dims = length(payload))
    writeDataSet(dhandle, payload)
    closeDataSet(dhandle)

    dhandle <- openDataSet(fhandle, "foo")
    reloaded <- readDataSet(dhandle)
    expect_identical(reloaded$value, substr(payload, 1, 3))
    closeDataSet(dhandle)

    dhandle <- openDataSet(fhandle, "bar")
    reloaded <- readDataSet(dhandle)
    expect_identical(reloaded$value, payload)
    closeDataSet(dhandle)

    closeGroup(fhandle)
})

test_that("readDataSet error on too-large integers", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    payload <- (2^53 + 1:10 * 10)
    dhandle <- createDataSet(fhandle, "bar", type = createIntegerType(64, TRUE), dims = 10)
    writeDataSet(dhandle, payload)
    expect_error(readDataSet(dhandle), "overflow")
    writeDataSet(dhandle, -payload)
    expect_error(readDataSet(dhandle), "overflow")
    closeDataSet(dhandle)

    # Same for unsigned 64-bit integers.
    dhandle <- createDataSet(fhandle, "foo", type = createIntegerType(64, FALSE), dims = 10)
    writeDataSet(dhandle, payload)
    expect_error(readDataSet(dhandle), "overflow")
    closeDataSet(dhandle)

    closeGroup(fhandle)
})

