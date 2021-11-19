# Test summarise.R

# Setup -----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA

# Tests: cat_vcount -----------------------------------------------------------

test_that("cat_summarise rejects a data argument that is not a dataframe", {

    msg <- "The \"data\" argument is not a dataframe."
    expect_error(cat_summarise(NULL, "cyl", "mpg"), msg)
    expect_error(cat_summarise(NA, "cyl", "mpg"), msg)
    expect_error(cat_summarise(1:10, "cyl", "mpg"), msg)
    expect_error(cat_summarise(LETTERS[1:10], "cyl", "mpg"), msg)
    expect_error(cat_summarise(c(TRUE, FALSE), "cyl", "mpg"), msg)
    expect_error(cat_summarise(list(), "cyl", "mpg"), msg)
})

test_that("cat_summarise rejects a data argument that has no rows", {

    msg <- "The \"data\" argument is empty."
    expect_error(cat_summarise(data.frame(), "cyl", "mpg"), msg)
})

test_that("cat_summarise rejects invalid cat arguments", {

    msg <- "Invalid \"cat\" argument. Must be a character vector of length one."
    expect_error(cat_summarise(data, NULL, "mpg"), msg)
    expect_error(cat_summarise(data, NA, "mpg"), msg)
    expect_error(cat_summarise(data, 1:10, "mpg"), msg)
    expect_error(cat_summarise(data, LETTERS[1:10], "mpg"), msg)
    expect_error(cat_summarise(data, c(TRUE, FALSE), "mpg"), msg)
    expect_error(cat_summarise(data, list(), "mpg"), msg)
})

test_that("cat_summarise rejects a cat argument that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_summarise(data, "notacolumn", "mpg"), msg)
})

test_that("cat_summarise rejects invalid num arguments", {

    msg <- "Invalid \"num\" argument. Must be a character vector of length one."
    expect_error(cat_summarise(data, "cyl", NULL), msg)
    expect_error(cat_summarise(data, "cyl", NA), msg)
    expect_error(cat_summarise(data, "cyl", 1:10), msg)
    expect_error(cat_summarise(data, "cyl", LETTERS[1:10]), msg)
    expect_error(cat_summarise(data, "cyl", c(TRUE, FALSE)), msg)
    expect_error(cat_summarise(data, "cyl", list()), msg)
})

test_that("cat_summarise rejects a num argument that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_summarise(data, "cyl", "notacolumn"), msg)
})

test_that("cat_summarise rejects a num argument that is not a numeric column", {

    msg <- "The num argument is not a numeric column."
    expect_error(cat_summarise(data, "cyl", "manufacturer"), msg)
})

test_that("cat_summarise rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = NULL), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = NA), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = 1), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = ""), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = 1:10), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = LETTERS[1:10]), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = c(TRUE, FALSE)), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", na.rm = list()), msg)
})

test_that("cat_summarise rejects invalid clean_names arguments", {

    msg <- "Invalid \"clean_names\" argument. Must be either TRUE or FALSE."
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = NULL), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = NA), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = 1), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = ""), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = 1:10), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = LETTERS[1:10]), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = c(TRUE, FALSE)), msg)
    expect_error(cat_summarise(data, "cyl", "mpg", clean_names = list()), msg)
})

test_that("cat_summarise returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "cyl", "mpg")
    expect_equal(observed, expected)

    data_na_all_na <- data
    data_na_all_na$mpg[1] <- NA

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 1),
        mean = c(26.663636, 19.5333333, 15.10000, NA),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, NA),
        lq = c(22.800, 18.375, 14.400, NA),
        med = c(26.00, 19.45, 15.20, NA),
        uq = c(30.400, 20.675, 16.250, NA),
        max = c(33.9, 21.4, 19.2, NA))
    observed <- cat_summarise(data_na_all_na, "cyl", "mpg")
    expect_equal(observed, expected)

    data_na_summary <- data
    data_na_summary$cyl[3] <- NA

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(10, 6, 14, 2),
        na = c(0, 0, 0, 0),
        mean = c(27.05000, 19.5333333, 15.10000, 21.90000),
        sd = c(4.557838182, 1.4719601444, 2.5600480765, 1.272792061),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(23.200, 18.375, 14.400, 21.450),
        med = c(26.65, 19.45, 15.20, 21.90),
        uq = c(30.400, 20.675, 16.250, 22.350),
        max = c(33.9, 21.4, 19.2, 22.8))
    observed <- cat_summarise(data_na_summary, "cyl", "mpg")
    expect_equal(observed, expected)

})

test_that("cat_summarise returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "cyl", "mpg", na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(4, 6, 8),
        n = c(11, 6, 14),
        na = c(0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000),
        sd = c(4.50982765, 1.47196014, 2.56004808),
        min = c(21.4, 17.8, 10.4),
        lq = c(22.800, 18.375, 14.400),
        med = c(26.00, 19.45, 15.20),
        uq = c(30.400, 20.675, 16.250),
        max = c(33.9, 21.4, 19.2))
    observed <- cat_summarise(data, "cyl", "mpg", na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_summarise returns correct data with a valid clean_names argument", {

    data$Cyl <- data$cyl

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "Cyl", "mpg", clean_names = TRUE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        Cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "Cyl", "mpg", clean_names = FALSE)
    expect_equal(observed, expected)
})

test_that("cat_summarise uses option for default clean_names argument", {

    data$Cyl <- data$cyl
    restore_option <- getOption("tabbycat.clean_names")

    options(tabbycat.clean_names = FALSE)
    expected <- tibble::tibble(
        Cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "Cyl", "mpg")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = TRUE)
    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n = c(11, 6, 14, 1),
        na = c(0, 0, 0, 0),
        mean = c(26.663636, 19.5333333, 15.10000, 21.00000),
        sd = c(4.50982765, 1.47196014, 2.56004808, NA),
        min = c(21.4, 17.8, 10.4, 21.0),
        lq = c(22.800, 18.375, 14.400, 21.000),
        med = c(26.00, 19.45, 15.20, 21.00),
        uq = c(30.400, 20.675, 16.250, 21.000),
        max = c(33.9, 21.4, 19.2, 21.0))
    observed <- cat_summarise(data, "Cyl", "mpg")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = restore_option)
})

test_that("cat_summarize is an alias for cat_summarise", {

    summarise <- cat_summarise(data, "cyl", "mpg")
    summarize <- cat_summarize(data, "cyl", "mpg")
    expect_equal(summarise, summarize)
})
