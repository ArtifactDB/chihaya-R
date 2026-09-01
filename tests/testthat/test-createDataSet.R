# library(testthat); library(chihaya); source("test-createDataSet.R")

test_that("createDataSet works correctly for all types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    for (sign in c(TRUE, FALSE)) {
        for (bits in c(8, 16, 32, 64)) {
            dname <- paste0("bar_", if (sign) "U" else "I", bits)
            dhandle <- createDataSet(fhandle, dname, type = createIntegerType(bits, sign = sign), dims = c(20, 5))
            writeDataSet(dhandle, 1:100)
            closeDataSet(dhandle)
            expect_true(childExists(fhandle, dname))
        }
    }

    for (bits in c(32, 64)) {
        dname <- paste0("foo_F", bits)
        dhandle <- createDataSet(fhandle, dname, type = createFloatType(bits), dims = c(1, 2, 3))
        writeDataSet(dhandle, pi * 1:6)
        closeDataSet(dhandle)
        expect_true(childExists(fhandle, dname))
    }

    for (strlen in c(0, 10)) {
        dname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen))
        dhandle <- createDataSet(fhandle, dname, type = createStringType(strlen, "ASCII"), dims = 10)
        writeDataSet(dhandle, paste0("FOO_", 1:10))
        closeDataSet(dhandle)
        expect_true(childExists(fhandle, dname))
    }

    closeGroup(fhandle)
})

test_that("createDataSet works for scalars", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    dname <- "foo"
    dhandle <- createDataSet(fhandle, dname, type = createStringType(10, "ASCII"), dims = NULL)
    writeDataSet(dhandle, "FOOBAR")
    closeDataSet(dhandle)
    expect_true(childExists(fhandle, dname))

    closeGroup(fhandle)
})

test_that("createDataSet works correctly without compression", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    dname <- "FOO"
    dhandle <- createDataSet(fhandle, dname, type = createFloatType(64), dims = c(20, 5), compress = 0)
    writeDataSet(dhandle, runif(100))
    closeDataSet(dhandle)
    expect_true(childExists(fhandle, dname))

    # Automatically turns off compression if any extent is zero.
    dname <- "BAR"
    dhandle <- createDataSet(fhandle, dname, type = createFloatType(64), dims = c(20, 0))
    closeDataSet(dhandle)
    expect_true(childExists(fhandle, dname))

    closeGroup(fhandle)
})

test_that("createDataSet caps the chunk sizes", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)

    dname <- "FOO"
    expect_error(createDataSet(fhandle, dname, type = createIntegerType(16, TRUE), dims = c(20, 5), chunkdims = 50), "same length")

    dhandle <- createDataSet(fhandle, dname, type = createIntegerType(16, TRUE), dims = c(20, 5), chunkdims = c(50, 20))
    writeDataSet(dhandle, sample(100))
    closeDataSet(dhandle)
    expect_true(childExists(fhandle, dname))

    closeGroup(fhandle)
})
