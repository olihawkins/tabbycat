# Test vcount.R

# Setup -----------------------------------------------------------------------

cat <- c("a", "b", "b", "c", "c", "c", "d", "d", "d", "d")

# Tests: cat_vcount -----------------------------------------------------------

test_that("cat_vcount rejects a cat argument that is not a vector", {

    msg <- "The \"cat\" argument is not a vector."
    expect_error(cat_vcount(NULL), msg)
    expect_error(cat_vcount(list()), msg)
    expect_error(cat_vcount(data.frame()), msg)
})

test_that("cat_vcount rejects a cat argument that is an empty factor", {

    msg <- "The \"cat\" argument is empty."
    expect_error(cat_vcount(factor()), msg)
})

test_that("cat_vcount rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_vcount(cat, na.rm = NULL), msg)
    expect_error(cat_vcount(cat, na.rm = NA), msg)
    expect_error(cat_vcount(cat, na.rm = 1), msg)
    expect_error(cat_vcount(cat, na.rm = ""), msg)
    expect_error(cat_vcount(cat, na.rm = 1:10), msg)
    expect_error(cat_vcount(cat, na.rm = LETTERS[1:10]), msg)
    expect_error(cat_vcount(cat, na.rm = c(TRUE, FALSE)), msg)
    expect_error(cat_vcount(cat, na.rm = list()), msg)
})

test_that("cat_vcount rejects invalid clean_names arguments", {

    msg <- "Invalid \"clean_names\" argument. Must be either TRUE or FALSE."
    expect_error(cat_vcount(cat, clean_names = NULL), msg)
    expect_error(cat_vcount(cat, clean_names = NA), msg)
    expect_error(cat_vcount(cat, clean_names = 1), msg)
    expect_error(cat_vcount(cat, clean_names = ""), msg)
    expect_error(cat_vcount(cat, clean_names = 1:10), msg)
    expect_error(cat_vcount(cat, clean_names = LETTERS[1:10]), msg)
    expect_error(cat_vcount(cat, clean_names = c(TRUE, FALSE)), msg)
    expect_error(cat_vcount(cat, clean_names = list()), msg)
})

test_that("cat_vcount rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a character vector of length one."
    expect_error(cat_vcount(cat, only = NULL), msg)
    expect_error(cat_vcount(cat, only = NA), msg)
    expect_error(cat_vcount(cat, only = 1), msg)
    expect_error(cat_vcount(cat, only = TRUE), msg)
    expect_error(cat_vcount(cat, only = 1:10), msg)
    expect_error(cat_vcount(cat, only = LETTERS[1:10]), msg)
    expect_error(cat_vcount(cat, only = c(TRUE, FALSE)), msg)
    expect_error(cat_vcount(cat, only = list()), msg)
})

test_that("cat_vcount returns expected data with defaults", {

    expected <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(cat)
    expect_equal(observed, expected)
})

test_that("cat_vcount returns expected data with a valid na.rm argument", {

    cat <- c(cat, NA)

    expected <- tibble::tibble(
        cat = c(letters[4:1], NA),
        number = as.integer(c(4:1, 1)),
        percent = number / sum(number))
    observed <- cat_vcount(cat)
    expect_equal(observed[2:3], expected[2:3])
    expect_equal(observed[1:4, ], expected[1:4, ])
    expect_equal(is.na(observed[[5, 1]]), TRUE)

    expected <- tibble::tibble(
        cat = c(letters[4:1], NA),
        number = as.integer(c(4:1, 1)),
        percent = number / sum(number))
    observed <- cat_vcount(cat, na.rm = FALSE)
    expect_equal(observed[2:3], expected[2:3])
    expect_equal(observed[1:4, ], expected[1:4, ])
    expect_equal(is.na(observed[[5, 1]]), TRUE)

    expected <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(cat, na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_count returns correct data with a valid clean_names argument", {

    Cat <- cat

    expected <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(Cat, clean_names = TRUE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        Cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(Cat, clean_names = FALSE)
    expect_equal(observed, expected)
})

test_that("cat_vcount uses option for default clean_names argument", {

    Cat <- cat
    restore_option <- getOption("tabbycat.clean_names")

    options(tabbycat.clean_names = TRUE)
    expected <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(Cat)
    expect_equal(observed, expected)

    options(tabbycat.clean_names = FALSE)
    expected <- tibble::tibble(
        Cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(Cat)
    expect_equal(observed, expected)

    options(tabbycat.clean_names = restore_option)
})

test_that("cat_vcount returns expected data with a valid only argument", {

    expected <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    observed <- cat_vcount(cat, only = "ignore")
    expect_equal(observed, expected)

    expected_number <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1)
    observed <- cat_vcount(cat, only = "n")
    expect_equal(observed, expected_number)

    observed <- cat_vcount(cat, only = "number")
    expect_equal(observed, expected_number)

    observed <- cat_vcount(cat, only = " number ")
    expect_equal(observed, expected_number)

    expected_percent <- tibble::tibble(
        cat = letters[4:1],
        percent = 4:1 / sum(4:1))

    observed <- cat_vcount(cat, only = "p")
    expect_equal(observed, expected_percent)

    observed <- cat_vcount(cat, only = "percent")
    expect_equal(observed, expected_percent)

    observed <- cat_vcount(cat, only = " percent ")
    expect_equal(observed, expected_percent)
})

