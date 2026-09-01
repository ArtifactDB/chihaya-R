# library(testthat); library(chihaya); source("test-createAttribute.R")

test_that("createAttribute works correctly for groups", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    for (sign in c(TRUE, FALSE)) {
        for (bits in c(8, 16, 32, 64)) {
            aname <- paste0("bar_", if (sign) "U" else "I", bits)
            ahandle <- createAttribute(ghandle, aname, type = createIntegerType(bits, sign = sign), dims = c(2, 3, 4))
            writeAttribute(ahandle, 1:24)
            closeAttribute(ahandle)
            expect_true(attributeExists(ghandle, aname))
        }
    }

    for (bits in c(32, 64)) {
        aname <- paste0("foo_F", bits)
        ahandle <- createAttribute(ghandle, aname, type = createFloatType(bits), dims = 10)
        writeAttribute(ahandle, 1:10 * pi)
        closeAttribute(ahandle)
        expect_true(attributeExists(ghandle, aname))
    }

    for (strlen in c(0, 10)) {
        aname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen))
        ahandle <- createAttribute(ghandle, aname, type = createStringType(strlen), dims = c(2,3))
        writeAttribute(ahandle, paste0("FOO_", 1:6))
        closeAttribute(ahandle)
        expect_true(attributeExists(ghandle, aname))
    }

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("createAttribute works for scalars in groups", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    aname <- "bar"
    ahandle <- createAttribute(ghandle, aname, type = createIntegerType(32, sign = FALSE), dims = NULL)
    writeAttribute(ahandle, 99)
    closeAttribute(ahandle)
    expect_true(attributeExists(ghandle, aname))

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("createAttribute works correctly for datasets", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(32, sign = FALSE), dims = c(9, 3))

    for (sign in c(TRUE, FALSE)) {
        for (bits in c(8, 16, 32, 64)) {
            aname <- paste0("bar_", if (sign) "U" else "I", bits)
            ahandle <- createAttribute(dhandle, aname, type = createIntegerType(bits, sign = sign), dims = c(10, 20))
            writeAttribute(ahandle, 200:1)
            closeAttribute(ahandle)
            expect_true(attributeExists(dhandle, aname))
        }
    }

    for (bits in c(32, 64)) {
        aname <- paste0("foo_F", bits)
        ahandle <- createAttribute(dhandle, aname, type = createFloatType(bits), dims = c(5, 2))
        writeAttribute(ahandle, 1:10 * pi)
        closeAttribute(ahandle)
        expect_true(attributeExists(dhandle, aname))
    }

    for (strlen in c(0, 10)) {
        aname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen))
        ahandle <- createAttribute(dhandle, aname, type = createStringType(strlen), dims = 5)
        writeAttribute(ahandle, paste0("FOO_", 1:5))
        closeAttribute(ahandle)
        expect_true(attributeExists(dhandle, aname))
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("createAttribute works for scalars in datasets", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createFloatType(64), dims = c(10, 20))

    aname <- "bar"
    ahandle <- createAttribute(dhandle, aname, type = createIntegerType(8, sign = FALSE), dims = NULL)
    writeAttribute(ahandle, FALSE)
    closeAttribute(ahandle)
    expect_true(attributeExists(dhandle, aname))

    closeDataSet(dhandle)
    closeGroup(fhandle)
})
