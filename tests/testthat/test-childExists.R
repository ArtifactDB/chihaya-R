# library(testthat); library(chihaya); source("test-childExists.R")

test_that("childExists works correctly", {
    tmp <- tempfile(fileext = ".h5")

    {
        fhandle <- createFile(tmp)
        ghandle <- createGroup(fhandle, "foo")
        dhandle <- createDataSet(fhandle, "bar", type = createFloatType(64), dims = c(10, 5))

        expect_true(childExists(fhandle, "foo"))
        expect_true(childExists(fhandle, "bar"))
        expect_false(childExists(fhandle, "whee"))

        expect_identical(childExists(fhandle, "foo", report.type = TRUE), "group")
        expect_identical(childExists(fhandle, "bar", report.type = TRUE), "dataset")
        expect_null(childExists(fhandle, "whee", report.type = TRUE))

        closeDataSet(dhandle)
        closeGroup(ghandle)
        closeGroup(fhandle)
    }

    {
        fhandle <- openFile(tmp)

        expect_true(childExists(fhandle, "foo"))
        expect_true(childExists(fhandle, "bar"))
        expect_false(childExists(fhandle, "whee"))

        expect_identical(childExists(fhandle, "foo", report.type = TRUE), "group")
        expect_identical(childExists(fhandle, "bar", report.type = TRUE), "dataset")
        expect_null(childExists(fhandle, "whee", report.type = TRUE))

        closeGroup(fhandle)
    }
})
