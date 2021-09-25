# Test contrast.R

# Setup ----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA

# Tests: cat_contrast --------------------------------------------------------

test_that("cat_contrast rejects a data argument that is not a dataframe", {

    msg <- "The \"data\" argument is not a dataframe."
    expect_error(cat_contrast(NULL, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(NA, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(1:10, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(LETTERS[1:10], "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(c(TRUE, FALSE), "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(list(), "cyl", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a data argument that has no rows", {

    msg <- "The \"data\" argument is empty."
    expect_error(cat_contrast(data.frame(), "cyl", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects invalid row_cat arguments", {

    msg <- "The \"row_cat\" argument is not a character vector of length one."
    expect_error(cat_contrast(data, NULL, "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, NA, "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, 1:10, "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, LETTERS[1:10], "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, c(TRUE, FALSE), "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, list(), "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a row_cat argument that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_contrast(data, "notacolumn", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects invalid col_cat arguments", {

    msg <- "The \"col_cat\" argument is not a character vector of length one."
    expect_error(cat_contrast(data, "cyl", NULL, "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", NA, "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", 1:10, "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", LETTERS[1:10], "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", c(TRUE, FALSE), "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", list(), "Merc"), msg)
})

test_that("cat_contrast rejects a col_cat argument that is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_contrast(data, "cyl", "notacolumn", "Merc"), msg)
})

test_that("cat_contrast rejects invalid col_group arguments", {

    msg <- "The \"col_group\" argument is not a valid vector of length one."
    expect_error(cat_contrast(data, "cyl", "manufacturer", NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", list()), msg)
})

test_that("cat_contrast rejects a col_group argument that does not exist in col_cat", {

    msg <- "The \"col_group\" 'notagroup' does not exist in the \"col_cat\" 'manufacturer'."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "notagroup"), msg)
})

test_that("cat_contrast rejects invalid na.rm.row arguments", {

    msg <- "Invalid \"na.rm.row\" argument. Must be either TRUE or FALSE."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = ""), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.row = list()), msg)
})

test_that("cat_contrast rejects invalid na.rm.col arguments", {

    msg <- "Invalid \"na.rm.col\" argument. Must be either TRUE or FALSE."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = ""), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm.col = list()), msg)
})

test_that("cat_contrast rejects invalid clean_names arguments", {

    msg <- "Invalid \"clean_names\" argument. Must be either TRUE or FALSE."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = ""), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", clean_names = list()), msg)
})

test_that("cat_contrast rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a single string."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = TRUE), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = list()), msg)
})

# test_that("cat_contrast returns correct data with defaults", {
#
#     expected <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         n_merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc")
#     expect_equal(observed, expected)
# })
#
# test_that("cat_contrast returns correct data with a valid na.rm argument", {
#
#     expected <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         n_merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         na.rm = FALSE)
#     expect_equal(observed, expected)
#
#     expected <- tibble::tibble(
#         cyl = c(8, 4, 6),
#         n_merc = c(3, 2, 2),
#         n_other = c(11, 9, 4),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429),
#         p_other = c(0.45833333, 0.3750000, 0.166666667))
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         na.rm = TRUE)
#     expect_equal(observed, expected)
# })
#
# test_that("cat_contrast returns correct data with a valid clean_names argument", {
#
#     data$Cyl <- data$cyl
#
#     expected <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         n_merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#     observed <- cat_contrast(
#         data,
#         "Cyl",
#         "manufacturer",
#         "Merc",
#         clean_names = TRUE)
#     expect_equal(observed, expected)
#
#     expected <- tibble::tibble(
#         Cyl = c(8, 4, 6, NA),
#         n_Merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1),
#         p_Merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#     observed <- cat_contrast(
#         data,
#         "Cyl",
#         "manufacturer",
#         "Merc",
#         clean_names = FALSE)
#     expect_equal(observed, expected)
# })
#
# test_that("cat_contrast returns correct data with a valid only argument", {
#
#     expected <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         n_merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = "ignore")
#     expect_equal(observed, expected)
#
#     expected_number <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         n_merc = c(3, 2, 2, 0),
#         n_other = c(11, 9, 4, 1))
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = "n")
#     expect_equal(observed, expected_number)
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = "number")
#     expect_equal(observed, expected_number)
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = " number ")
#     expect_equal(observed, expected_number)
#
#     expected_percent <- tibble::tibble(
#         cyl = c(8, 4, 6, NA),
#         p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
#         p_other = c(0.44, 0.36, 0.16, 0.04))
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = "p")
#     expect_equal(observed, expected_percent)
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = "percent")
#     expect_equal(observed, expected_percent)
#
#     observed <- cat_contrast(
#         data,
#         "cyl",
#         "manufacturer",
#         "Merc",
#         only = " percent ")
#     expect_equal(observed, expected_percent)
# })
