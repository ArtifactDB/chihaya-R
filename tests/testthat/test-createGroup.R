# library(testthat); library(chihaya); source("test-createGroup.R")

test_that("createDataSet works correctly for all types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "foobar")
    expect_identical(childExists(fhandle, "foobar", report.type = TRUE), "group")
    closeGroup(ghandle)
    closeGroup(fhandle)

    fhandle <- openFile(tmp)
    expect_identical(childExists(fhandle, "foobar", report.type = TRUE), "group")
    closeGroup(fhandle)
})
