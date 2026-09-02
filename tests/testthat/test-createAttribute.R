# library(testthat); library(chihaya); source("test-createAttribute.R")

test_that("attributes work for groups, small integer types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    all.dims <- list(integer(0), 100L, c(2L, 3L, 4L))
    all.payloads <- list(99L, 100:1, 1:24)
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (type in list(
            createIntegerType(8, FALSE),
            createIntegerType(8, TRUE),
            createIntegerType(16, FALSE),
            createIntegerType(16, TRUE),
            createIntegerType(32, TRUE)
        )) {
            aname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", d)
            ahandle <- createAttribute(ghandle, aname, type = type, dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(ghandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, large integer types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    all.dims <- list(integer(0), 50L, c(6L, 4L))
    all.payloads <- list(99L, 1:50 * 10L, 48:25 * 2L)
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (type in list(
            createIntegerType(32, FALSE),
            createIntegerType(64, TRUE),
            createIntegerType(64, FALSE)
        )) {
            aname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", d)
            ahandle <- createAttribute(ghandle, aname, type = type, dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, as.double(payload))
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(ghandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, float types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    all.dims <- list(integer(0), 10L, c(3L, 2L, 4L))
    all.payloads <- list(1/128, 1/2^(1:10), -2^(-12:11))
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (bits in c(32, 64)) {
            aname <- paste0("foo_F", bits, "_", d)
            ahandle <- createAttribute(ghandle, aname, type = createFloatType(bits), dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(ghandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, string types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    all.dims <- list(integer(0), 10L, c(2L, 3L))
    all.payloads <- list("FOOBAR", paste0("FOO_", 1:10), paste0("BAR_", 1:6 * 1000))
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (strlen in c(0, 10)) {
            aname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen), "_", d)
            ahandle <- createAttribute(ghandle, aname, type = createStringType(strlen), dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(ghandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeGroup(ghandle)
    closeGroup(fhandle)
})

test_that("attributes work for dataset, small integer types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(), dims = NULL)

    all.dims <- list(integer(0), 100L, c(2L, 3L, 4L))
    all.payloads <- list(99L, 100:1, 1:24)
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (type in list(
            createIntegerType(8, FALSE),
            createIntegerType(8, TRUE),
            createIntegerType(16, FALSE),
            createIntegerType(16, TRUE),
            createIntegerType(32, TRUE)
        )) {
            aname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", d)
            ahandle <- createAttribute(dhandle, aname, type = type, dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(dhandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, large integer types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createFloatType(), dims = c(10, 20))

    all.dims <- list(integer(0), 50L, c(6L, 4L))
    all.payloads <- list(99L, 1:50 * 10L, 48:25 * 2L)
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (type in list(
            createIntegerType(32, FALSE),
            createIntegerType(64, TRUE),
            createIntegerType(64, FALSE)
        )) {
            aname <- paste0("bar_", if (type$sign) "U" else "I", type$bits, "_", d)
            ahandle <- createAttribute(dhandle, aname, type = type, dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, as.double(payload))
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(dhandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, float types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createStringType(), dims = 25)

    all.dims <- list(integer(0), 10L, c(3L, 2L, 4L))
    all.payloads <- list(1/128, 1/2^(1:10), -2^(-12:11))
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (bits in c(32, 64)) {
            aname <- paste0("foo_F", bits, "_", d)
            ahandle <- createAttribute(dhandle, aname, type = createFloatType(bits), dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(dhandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("attributes work for groups, string types", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(), dims = c(1,2,3))

    all.dims <- list(integer(0), 10L, c(2L, 3L))
    all.payloads <- list("FOOBAR", paste0("FOO_", 1:10), paste0("BAR_", 1:6 * 1000))
    for (d in seq_along(all.dims)) {
        dims <- all.dims[[d]]
        payload <- all.payloads[[d]]

        for (strlen in c(0, 10)) {
            aname <- paste0("stuff_", if (strlen == 0) "vlen" else paste0("s", strlen), "_", d)
            ahandle <- createAttribute(dhandle, aname, type = createStringType(strlen), dims = dims)
            writeAttribute(ahandle, payload)

            reloaded <- readAttribute(ahandle)
            expect_identical(reloaded$value, payload)
            expect_identical(reloaded$dims, dims)
            closeAttribute(ahandle)

            # Same result if we re-open it.
            ahandle2 <- openAttribute(dhandle, aname)
            reloaded2 <- readAttribute(ahandle2)
            expect_identical(reloaded, reloaded2)
            closeAttribute(ahandle)
        }
    }

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("attribute writing throws the right errors", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    dhandle <- createDataSet(fhandle, "blah", type = createIntegerType(), dims = c(1,2,3))

    ahandle <- createAttribute(dhandle, "foo", type = createIntegerType(), dims = 10)
    expect_error(writeAttribute(ahandle, 1:5), "not consistent")
    closeAttribute(ahandle)

    ahandle <- createAttribute(dhandle, "bar", type = createFloatType(), dims = c(5, 2))
    expect_error(writeAttribute(ahandle, runif(6)), "not consistent")
    closeAttribute(ahandle)

    ahandle <- createAttribute(dhandle, "stuff", type = createIntegerType(8, FALSE), dims = c(3, 2, 1))
    expect_error(writeAttribute(ahandle, c(TRUE, FALSE)), "not consistent")
    closeAttribute(ahandle)

    ahandle <- createAttribute(dhandle, "whee", type = createStringType(10), dims = c(3, 9))
    expect_error(writeAttribute(ahandle, "FOOBAR"), "not consistent")
    expect_error(writeAttribute(ahandle, c(LETTERS, NA)), "missing strings")
    closeAttribute(ahandle)

    closeDataSet(dhandle)
    closeGroup(fhandle)
})

test_that("attribute writing truncates fixed-length strings", {
    tmp <- tempfile(fileext = ".h5")
    fhandle <- createFile(tmp)
    ghandle <- createGroup(fhandle, "blah")

    payload <- c("akira", "ai", "akari", "alice", "athena", "alicia", "aika")
    ahandle <- createAttribute(ghandle, "foo", type = createStringType(3), dims = length(payload))
    writeAttribute(ahandle, payload)
    closeAttribute(ahandle)

    ahandle <- createAttribute(ghandle, "bar", type = createStringType(NULL), dims = length(payload))
    writeAttribute(ahandle, payload)
    closeAttribute(ahandle)

    ahandle <- openAttribute(ghandle, "foo")
    reloaded <- readAttribute(ahandle)
    expect_identical(reloaded$value, substr(payload, 1, 3))
    closeAttribute(ahandle)

    ahandle <- openAttribute(ghandle, "bar")
    reloaded <- readAttribute(ahandle)
    expect_identical(reloaded$value, payload)
    closeAttribute(ahandle)

    closeGroup(ghandle)
    closeGroup(fhandle)
})
