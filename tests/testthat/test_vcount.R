# Test vcount.R

# Setup -----------------------------------------------------------------------

cat <- c("a", "b", "b", "c", "c", "c", "d", "d", "d", "d")

# Tests: cat_vcount -----------------------------------------------------------

test_that("cat_vcount rejects a cat argument that is not a vector", {

    msg <- "The cat argument is not a vector."
    expect_error(cat_vcount(NULL), msg)
    expect_error(cat_vcount(list()), msg)
    expect_error(cat_vcount(data.frame()), msg)
})

test_that("cat_vcount rejects a cat argument that is an empty factor", {

    msg <- "The cat argument is empty."
    expect_error(cat_vcount(factor()), msg)
})

test_that("cat_vcount rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_vcount(cat, na.rm = NULL), msg)
    expect_error(cat_vcount(cat, na.rm = NA), msg)
    expect_error(cat_vcount(cat, na.rm = list()), msg)
    expect_error(cat_vcount(cat, na.rm = data.frame()), msg)
    expect_error(cat_vcount(cat, na.rm = 1), msg)
    expect_error(cat_vcount(cat, na.rm = ""), msg)
})

test_that("cat_vcount rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a single string."
    expect_error(cat_vcount(cat, only = NULL), msg)
    expect_error(cat_vcount(cat, only = NA), msg)
    expect_error(cat_vcount(cat, only = list()), msg)
    expect_error(cat_vcount(cat, only = data.frame()), msg)
    expect_error(cat_vcount(cat, only = 1), msg)
    expect_error(cat_vcount(cat, only = TRUE), msg)
    expect_error(cat_vcount(cat, only = c("n", "p")), msg)
})

test_that("cat_vcount returns correct data with defaults", {

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat)
    expect_equal(output, correct)
})

test_that("cat_vcount returns correct data with a valid na.rm argument", {

    cat <- c(cat, NA)

    correct <- tibble::tibble(
        cat = c(letters[4:1], NA),
        number = as.integer(c(4:1, 1)),
        percent = number / sum(number))
    output <- cat_vcount(cat)
    expect_equal(output[2:3], correct[2:3])
    expect_equal(output[1:4, ], correct[1:4, ])
    expect_equal(is.na(output[[5, 1]]), TRUE)

    correct <- tibble::tibble(
        cat = c(letters[4:1], NA),
        number = as.integer(c(4:1, 1)),
        percent = number / sum(number))
    output <- cat_vcount(cat, na.rm = FALSE)
    expect_equal(output[2:3], correct[2:3])
    expect_equal(output[1:4, ], correct[1:4, ])
    expect_equal(is.na(output[[5, 1]]), TRUE)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, na.rm = TRUE)
    expect_equal(output, correct)
})

test_that("cat_vcount returns correct data with a valid only argument", {

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, only = "ignore")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1)
    output <- cat_vcount(cat, only = "n")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1)
    output <- cat_vcount(cat, only = "number")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1)
    output <- cat_vcount(cat, only = " number ")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        percent = 4:1 / sum(4:1))
    output <- cat_vcount(cat, only = "p")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        percent = 4:1 / sum(4:1))
    output <- cat_vcount(cat, only = "percent")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        percent = 4:1 / sum(4:1))
    output <- cat_vcount(cat, only = " percent ")
    expect_equal(output, correct)
})

