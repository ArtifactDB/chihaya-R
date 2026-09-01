# library(testthat); library(chihaya); source("test-createAttribute.R")

test_that("createAttribute works correctly for groups", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    for (sign in c(TRUE, FALSE)) {
        for (bits in c(8, 16, 32, 64)) {
            aname <- paste0("bar_", if (sign) "U" else "I", bits)
            ahandle <- createAttribute(ghandle, aname, type = createIntegerType(bits, sign = sign), dims = NULL)
            writeAttribute(ahandle, 99L)
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

test_that("createAttribute works correctly for datasets", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(32, sign = FALSE), dims = c(9, 3))

    for (sign in c(TRUE, FALSE)) {
        for (bits in c(8, 16, 32, 64)) {
            aname <- paste0("bar_", if (sign) "U" else "I", bits)
            ahandle <- createAttribute(dhandle, aname, type = createIntegerType(bits, sign = sign), dims = NULL)
            writeAttribute(ahandle, 99L)
            closeAttribute(ahandle)
            expect_true(attributeExists(dhandle, aname))
        }
    }

    for (bits in c(32, 64)) {
        aname <- paste0("foo_F", bits)
        ahandle <- createAttribute(dhandle, aname, type = createFloatType(bits), dims = 10)
        writeAttribute(ahandle, 1:10 * pi)
        closeAttribute(ahandle)
        expect_true(attributeExists(dhandle, aname))
    }

    for (strlen in c(0, 10)) {
        aname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen))
        ahandle <- createAttribute(dhandle, aname, type = createStringType(strlen), dims = c(2,3))
        writeAttribute(ahandle, paste0("FOO_", 1:6))
        closeAttribute(ahandle)
        expect_true(attributeExists(dhandle, aname))
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})
