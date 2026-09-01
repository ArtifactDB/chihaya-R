# library(testthat); library(chihaya); source("test-attributeExists.R")

test_that("attributeExists works correctly for groups", {
    tmp <- tempfile(fileext = ".h5")

    {
        fhandle <- createFile(tmp)
        ghandle <- createGroup(fhandle, "blah")
        ahandle <- createAttribute(ghandle, "bar", type = createIntegerType(32, sign = TRUE), dims = NULL)
        closeAttribute(ahandle)
        expect_true(attributeExists(ghandle, "bar"))
        expect_false(attributeExists(ghandle, "foo"))
        closeGroup(ghandle)
        closeGroup(fhandle)
    }

    # Same result if we just open the file.
    {
        fhandle <- openFile(tmp)
        ghandle <- openGroup(fhandle, "blah")
        expect_true(attributeExists(ghandle, "bar"))
        expect_false(attributeExists(ghandle, "foo"))
        closeGroup(ghandle)
        closeGroup(fhandle)
    }
})

test_that("attributeExists works correctly for datasets", {
    tmp <- tempfile(fileext = ".h5")

    {
        fhandle <- createFile(tmp)
        dhandle <- createDataSet(fhandle, "stuff", type = createIntegerType(16, sign = FALSE), dims = c(10, 15))
        ahandle <- createAttribute(dhandle, "whee", type = createIntegerType(32, sign = TRUE), dims = NULL)
        closeAttribute(ahandle)
        expect_true(attributeExists(dhandle, "whee"))
        expect_false(attributeExists(dhandle, "foo"))
        closeDataSet(dhandle)
        closeGroup(fhandle)
    }

    # Same result if we just open the file.
    {
        fhandle <- openFile(tmp)
        dhandle <- openDataSet(fhandle, "stuff")
        expect_true(attributeExists(dhandle, "whee"))
        expect_false(attributeExists(dhandle, "foo"))
        closeDataSet(dhandle)
        closeGroup(fhandle)
    }
})
