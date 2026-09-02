# library(testthat); library(chihaya); source("test-listChildren.R")

test_that("listChildren works correctly", {
    tmp <- tempfile(fileext = ".h5")

    {
        fhandle <- createFile(tmp)
        ghandle <- createGroup(fhandle, "foo")
        closeGroup(ghandle)
        ghandle <- createGroup(fhandle, "bar")
        closeGroup(ghandle)
        dhandle <- createDataSet(fhandle, "whee", type = createFloatType(64), dims = c(10, 5))
        closeDataSet(dhandle)
        dhandle <- createDataSet(fhandle, "stuff", type = createIntegerType(), dims = NULL)
        closeDataSet(dhandle)

        info <- listChildren(fhandle)
        expect_identical(sort(info$group), c("bar", "foo"))
        expect_identical(sort(info$dataset), c("stuff", "whee"))

        closeGroup(fhandle)
    }

    {
        fhandle <- openFile(tmp)
        info <- listChildren(fhandle)
        expect_identical(sort(info$group), c("bar", "foo"))
        expect_identical(sort(info$dataset), c("stuff", "whee"))
        closeGroup(fhandle)
    }
})
