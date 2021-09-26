# Test compare.R

# Setup ----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA
data$vs[2] <- NA

# Tests: cat_compare --------------------------------------------------------

test_that("cat_compare rejects a data argument that is not a dataframe", {

    msg <- "The \"data\" argument is not a dataframe."
    expect_error(cat_compare(NULL, "cyl", "vs"), msg)
    expect_error(cat_compare(NA, "cyl", "vs"), msg)
    expect_error(cat_compare(1:10, "cyl", "vs"), msg)
    expect_error(cat_compare(LETTERS[1:10], "cyl", "vs"), msg)
    expect_error(cat_compare(c(TRUE, FALSE), "cyl", "vs"), msg)
    expect_error(cat_compare(list(), "cyl", "vs"), msg)
})

test_that("cat_compare rejects a data argument that has no rows", {

    msg <- "The \"data\" argument is empty."
    expect_error(cat_compare(data.frame(), "cyl", "vs"), msg)
})

test_that("cat_compare rejects invalid row_cat arguments", {

    msg <- "Invalid \"row_cat\" argument. Must be a character vector of length one."
    expect_error(cat_compare(data, NULL, "vs"), msg)
    expect_error(cat_compare(data, NA, "vs"), msg)
    expect_error(cat_compare(data, 1:10, "vs"), msg)
    expect_error(cat_compare(data, LETTERS[1:10], "vs"), msg)
    expect_error(cat_compare(data, c(TRUE, FALSE), "vs"), msg)
    expect_error(cat_compare(data, list(), "vs"), msg)
})

test_that("cat_compare rejects a row_cat argument that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_compare(data, "notacolumn", "vs"), msg)
})

test_that("cat_compare rejects invalid col_cat arguments", {

    msg <- "Invalid \"col_cat\" argument. Must be a character vector of length one."
    expect_error(cat_compare(data, "cyl", NULL), msg)
    expect_error(cat_compare(data, "cyl", NA), msg)
    expect_error(cat_compare(data, "cyl", 1:10), msg)
    expect_error(cat_compare(data, "cyl", LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", list()), msg)
})

test_that("cat_compare rejects a col_cat argument  that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_compare(data, "cyl", "notacolumn"), msg)
})

test_that("cat_compare rejects invalid na.rm.row arguments", {

    msg <- "Invalid \"na.rm.row\" argument. Must be either TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = ""), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.row = list()), msg)
})

test_that("cat_compare rejects invalid na.rm.col arguments", {

    msg <- "Invalid \"na.rm.col\" argument. Must be either TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = ""), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm.col = list()), msg)
})

test_that("cat_compare rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either NULL, TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", na.rm = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = ""), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = list()), msg)
})

test_that("cat_compare rejects invalid clean_names arguments", {

    msg <- "Invalid \"clean_names\" argument. Must be either TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", clean_names = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = ""), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = list()), msg)
})

test_that("cat_compare rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a character vector of length one."
    expect_error(cat_compare(data, "cyl", "vs", only = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = TRUE), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = list()), msg)
})

test_that("cat_compare rejects invalid na_label arguments", {

    msg <- "Invalid \"na_label\" argument. Must be a character vector of length one."
    expect_error(cat_compare(data, "cyl", "vs", na_label = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = TRUE), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", na_label = list()), msg)
})

test_that("cat_compare returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs")
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid na.rm.row argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs", na.rm.row = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(4, 6, 8),
        n_0 = c(1, 1, 14),
        n_1 = c(10, 4, 0),
        n_na = c(0, 1, 0),
        p_0 = c(0.0625, 0.0625, 0.8750),
        p_1 = c(.71428571, 0.28571429, 0.0),
        p_na = c(0, 1, 0))
    observed <- cat_compare(data, "cyl", "vs", na.rm.row = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid na.rm.col argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs", na.rm.col = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(data, "cyl", "vs", na.rm.col = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs", na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(4, 6, 8),
        n_0 = c(1, 1, 14),
        n_1 = c(10, 4, 0),
        p_0 = c(0.0625, 0.0625, 0.8750),
        p_1 = c(.71428571, 0.28571429, 0.0))
    observed <- cat_compare(data, "cyl", "vs", na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid only argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs", only = "ignore")
    expect_equal(observed, expected)

    expected_number <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0))

    observed <- cat_compare(data, "cyl", "vs", only = "n")
    expect_equal(observed, expected_number)

    observed <- cat_compare(data, "cyl", "vs", only = "number")
    expect_equal(observed, expected_number)

    observed <- cat_compare(data, "cyl", "vs", only = " number ")
    expect_equal(observed, expected_number)

    expected_percent <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))

    observed <- cat_compare(data, "cyl", "vs", only = "p")
    expect_equal(observed, expected_percent)

    observed <- cat_compare(data, "cyl", "vs", only = "percent")
    expect_equal(observed, expected_percent)

    observed <- cat_compare(data, "cyl", "vs", only = " percent ")
    expect_equal(observed, expected_percent)
})

test_that("cat_compare returns correct data with a valid clean_names argument", {

    data$Cyl <- data$cyl

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "Cyl", "vs", clean_names = TRUE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        Cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "Cyl", "vs", clean_names = FALSE)
    expect_equal(observed, expected)
})

test_that("cat_compare uses option for default clean_names argument", {

    data$Cyl <- data$cyl
    restore_option <- getOption("tabbycat.clean_names")

    options(tabbycat.clean_names = TRUE)
    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "Cyl", "vs")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = FALSE)
    expected <- tibble::tibble(
        Cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_na = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_na = c(0, 1, 0, 0))
    observed <- cat_compare(data, "Cyl", "vs")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = restore_option)
})

test_that("cat_contrast returns correct data with a valid na_label argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_missing = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_missing = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs", na_label = "missing")
    expect_equal(observed, expected)
})

test_that("cat_compare uses option for default na_label argument", {

    restore_option <- getOption("tabbycat.na_label")

    options(tabbycat.na_label = "missing")
    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 1, 14, 1),
        n_1 = c(10, 4, 0, 0),
        n_missing = c(0, 1, 0, 0),
        p_0 = c(0.05882353, 0.05882353, 0.82352941, 0.05882353),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0),
        p_missing = c(0, 1, 0, 0))
    observed <- cat_compare(data, "cyl", "vs")
    expect_equal(observed, expected)

    options(tabbycat.na_label = restore_option)
})
