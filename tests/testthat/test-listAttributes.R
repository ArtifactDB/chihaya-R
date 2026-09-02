# library(testthat); library(chihaya); source("test-listAttributes.R")

test_that("listAttributes works as expected for groups", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    ahandle <- createAttribute(ghandle, "bar", type = createIntegerType(), dims = 2)
    closeAttribute(ahandle)
    ahandle <- createAttribute(ghandle, "foo", type = createStringType(), dims = c(2, 3))
    closeAttribute(ahandle)
    ahandle <- createAttribute(ghandle, "stuff", type = createFloatType(), dims = c(2, 3, 1))
    closeAttribute(ahandle)

    expect_identical(sort(listAttributes(ghandle)), c("bar", "foo", "stuff"))

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("listAttributes works as expected for datasets", {
    tmp <- tempfile(fileext = ".h5")

    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(), dims = c(10, 20))

    ahandle <- createAttribute(dhandle, "bar", type = createStringType(), dims = NULL)
    closeAttribute(ahandle)
    ahandle <- createAttribute(dhandle, "foo", type = createFloatType(), dims = NULL)
    closeAttribute(ahandle)
    ahandle <- createAttribute(dhandle, "stuff", type = createIntegerType(), dims = NULL)
    closeAttribute(ahandle)

    expect_identical(sort(listAttributes(dhandle)), c("bar", "foo", "stuff"))

    closeDataSet(dhandle)
    closeGroup(fhandle)
})
