# library(testthat); library(chihaya); source("test-createType.R")

test_that("createStringType works", {
    stype <- createStringType(NULL)
    expect_identical(stype$type, "string")
    expect_identical(stype$length, 0L)
    expect_identical(stype$encoding, "UTF-8")

    ftype <- createStringType(10, "ASCII")
    expect_identical(ftype$type, "string")
    expect_identical(ftype$length, 10L)
    expect_identical(ftype$encoding, "ASCII")

    expect_error(createStringType(10, "something"), "UTF-8")
})

test_that("createIntegerType works", {
    itype <- createIntegerType(32, TRUE)
    expect_identical(itype$type, "integer")
    expect_identical(itype$bits, 32L)
    expect_true(itype$sign)

    itype <- createIntegerType(64, FALSE)
    expect_identical(itype$type, "integer")
    expect_identical(itype$bits, 64L)
    expect_false(itype$sign)

    expect_error(createIntegerType(10, FALSE), "32")
})

test_that("createFloatType works", {
    ftype <- createFloatType(32)
    expect_identical(ftype$type, "float")
    expect_identical(ftype$bits, 32L)

    expect_error(createFloatType(10), "32")
})
