# library(testthat); library(chihaya); source("test-childExists.R")

test_that("childExists works correctly", {
    tmp <- tempfile(fileext = ".h5")

    {
        fhandle <- createFile(tmp)
        ghandle <- createGroup(fhandle, "foo")
        dhandle <- createDataSet(fhandle, "bar", type = createFloatType(64), dims = c(10, 5))
        expect_identical(childExists(fhandle, "foo"), "group")
        expect_identical(childExists(fhandle, "bar"), "dataset")
        expect_identical(childExists(fhandle, "whee"), "absent")
        closeDataSet(dhandle)
        closeGroup(ghandle)
        closeGroup(fhandle)
    }

    {
        fhandle <- openFile(tmp)
        expect_identical(childExists(fhandle, "foo"), "group")
        expect_identical(childExists(fhandle, "bar"), "dataset")
        expect_identical(childExists(fhandle, "whee"), "absent")
        closeGroup(fhandle)
    }
})
