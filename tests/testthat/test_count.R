# Test count.R

# Setup -----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA

# Tests: cat_vcount -----------------------------------------------------------

test_that("cat_count rejects a data argument that is not a dataframe", {

    msg <- "The data argument is not a dataframe."
    expect_error(cat_count(NULL, "cyl"), msg)
    expect_error(cat_count(NA, "cyl"), msg)
    expect_error(cat_count(1:10, "cyl"), msg)
    expect_error(cat_count(LETTERS[1:10], "cyl"), msg)
    expect_error(cat_count(c(TRUE, FALSE), "cyl"), msg)
    expect_error(cat_count(list(), "cyl"), msg)
})

test_that("cat_count rejects a data argument that has no rows", {

    msg <- "The data argument is empty."
    expect_error(cat_count(data.frame(), "cyl"), msg)
})

test_that("cat_count rejects a cat argument is NULL or NA", {

    msg <- "The cat argument is null."
    expect_error(cat_count(data, NULL), msg)
    expect_error(cat_count(data, NA), msg)
})

test_that("cat_count rejects a cat argument is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_count(data, "notacolumn"), msg)
})

test_that("cat_count rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_count(data, "cyl", na.rm = NULL), msg)
    expect_error(cat_count(data, "cyl", na.rm = NA), msg)
    expect_error(cat_count(data, "cyl", na.rm = list()), msg)
    expect_error(cat_count(data, "cyl", na.rm = data.frame()), msg)
    expect_error(cat_count(data, "cyl", na.rm = 1), msg)
    expect_error(cat_count(data, "cyl", na.rm = ""), msg)
})

test_that("cat_count rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a single string."
    expect_error(cat_count(data, "cyl", only = NULL), msg)
    expect_error(cat_count(data, "cyl", only = NA), msg)
    expect_error(cat_count(data, "cyl", only = list()), msg)
    expect_error(cat_count(data, "cyl", only = data.frame()), msg)
    expect_error(cat_count(data, "cyl", only = 1), msg)
    expect_error(cat_count(data, "cyl", only = TRUE), msg)
    expect_error(cat_count(data, "cyl", only = c("n", "p")), msg)
})

test_that("cat_count returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        number = c(14, 11, 6, 1),
        percent = c(0.43750, 0.34375, 0.18750, 0.03125))
    observed <- cat_count(data, "cyl")
    expect_equal(observed, expected)
})

test_that("cat_count returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        number = c(14, 11, 6, 1),
        percent = c(0.43750, 0.34375, 0.18750, 0.03125))
    observed <- cat_count(data, "cyl", na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(8, 4, 6),
        number = c(14, 11, 6),
        percent = c(0.4516129032, 0.3548387097, 0.1935483871))
    observed <- cat_count(data, "cyl", na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_count returns correct data with a valid only argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        number = c(14, 11, 6, 1),
        percent = c(0.43750, 0.34375, 0.18750, 0.03125))
    observed <- cat_count(data, "cyl", only = "ignore")
    expect_equal(observed, expected)

    expected_number <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        number = c(14, 11, 6, 1))

    observed <- cat_count(data, "cyl", only = "n")
    expect_equal(observed, expected_number)

    observed <- cat_count(data, "cyl", only = "number")
    expect_equal(observed, expected_number)

    observed <- cat_count(data, "cyl", only = " number ")
    expect_equal(observed, expected_number)

    expected_percent <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        percent = c(0.43750, 0.34375, 0.18750, 0.03125))

    observed <- cat_count(data, "cyl", only = "p")
    expect_equal(observed, expected_percent)

    observed <- cat_count(data, "cyl", only = "percent")
    expect_equal(observed, expected_percent)

    observed <- cat_count(data, "cyl", only = " percent ")
    expect_equal(observed, expected_percent)
})
