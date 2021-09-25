# Test compare.R

# Setup ----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA

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

    msg <- "The \"row_cat\" argument is not a character vector of length one."
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

    msg <- "The \"col_cat\" argument is not a character vector of length one."
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

    msg <- "Invalid \"only\" argument. Must be a single string."
    expect_error(cat_compare(data, "cyl", "vs", only = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = TRUE), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = 1:10), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = LETTERS[1:10]), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = c(TRUE, FALSE)), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = list()), msg)
})

# test_that("cat_compare returns correct data with defaults", {
#
#     expected <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs")
#     expect_equal(observed, expected)
# })
#
# test_that("cat_compare returns correct data with a valid na.rm argument", {
#
#     expected <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         na.rm = FALSE)
#     expect_equal(observed, expected)
#
#     expected <- tibble::tibble(
#         cyl = c(4, 6, 8),
#         n_0 = c(1, 2, 14),
#         n_1 = c(10, 4, 0),
#         p_0 = c(0.05882353, 0.11764706, 0.82352941),
#         p_1 = c(0.71428571, 0.28571429, 0.0000000))
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         na.rm = TRUE)
#     expect_equal(observed, expected)
# })
#
# test_that("cat_compare returns correct data with a valid clean_names argument", {
#
#     data$Cyl <- data$cyl
#
#     expected <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
#     observed <- cat_compare(
#         data,
#         "Cyl",
#         "vs",
#         clean_names = TRUE)
#     expect_equal(observed, expected)
#
#     expected <- tibble::tibble(
#         Cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
#     observed <- cat_compare(
#         data,
#         "Cyl",
#         "vs",
#         clean_names = FALSE)
#     expect_equal(observed, expected)
# })
#
# test_that("cat_compare returns correct data with a valid only argument", {
#
#     expected <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = "ignore")
#     expect_equal(observed, expected)
#
#     expected_number <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         n_0 = c(1, 2, 14, 1),
#         n_1 = c(10, 4, 0, 0))
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = "n")
#     expect_equal(observed, expected_number)
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = "number")
#     expect_equal(observed, expected_number)
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = " number ")
#     expect_equal(observed, expected_number)
#
#     expected_percent <- tibble::tibble(
#         cyl = c(4, 6, 8, NA),
#         p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
#         p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = "p")
#     expect_equal(observed, expected_percent)
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = "percent")
#     expect_equal(observed, expected_percent)
#
#     observed <- cat_compare(
#         data,
#         "cyl",
#         "vs",
#         only = " percent ")
#     expect_equal(observed, expected_percent)
# })
