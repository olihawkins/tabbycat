# Test contrast.R

# Setup ----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA
data$manufacturer[2] <- NA

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

    msg <- "Invalid \"row_cat\" argument. Must be a character vector of length one."
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

    msg <- "Invalid \"col_cat\" argument. Must be a character vector of length one."
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

    msg <- "Invalid \"col_group\" argument. Must be a character vector of length one."
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

test_that("cat_contrast rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either NULL, TRUE or FALSE."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = ""), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = list()), msg)
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

    msg <- "Invalid \"only\" argument. Must be a character vector of length one."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = TRUE), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", only = list()), msg)
})

test_that("cat_contrast rejects invalid other_label arguments", {

    msg <- "Invalid \"other_label\" argument. Must be a character vector of length one."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = TRUE), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", other_label = list()), msg)
})

test_that("cat_contrast rejects invalid na_label arguments", {

    msg <- "Invalid \"na_label\" argument. Must be a character vector of length one."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = TRUE), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = 1:10), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = LETTERS[1:10]), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = c(TRUE, FALSE)), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na_label = list()), msg)
})

test_that("cat_contrast returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(data, "cyl", "manufacturer", "Merc")
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid na.rm.row argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm.row = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(8, 4, 6),
        n_merc = c(3, 2, 2),
        n_other = c(11, 9, 3),
        n_na = c(0, 0, 1),
        p_merc = c( 0.42857143, 0.28571429, 0.28571429),
        p_other = c(0.47826087, 0.39130435, 0.13043478),
        p_na = c(0, 0, 1))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm.row = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid na.rm.col argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm.col = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm.col = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(8, 4, 6),
        n_merc = c(3, 2, 2),
        n_other = c(11, 9, 3),
        p_merc = c( 0.42857143, 0.28571429, 0.28571429),
        p_other = c(0.47826087, 0.39130435, 0.13043478))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid clean_names argument", {

    data$Cyl <- data$cyl

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "Cyl",
        "manufacturer",
        "Merc",
        clean_names = TRUE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        Cyl = c(8, 4, 6, NA),
        n_Merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_Merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "Cyl",
        "manufacturer",
        "Merc",
        clean_names = FALSE)
    expect_equal(observed, expected)
})

test_that("cat_contrast uses option for default clean_names argument", {

    data$Cyl <- data$cyl
    restore_option <- getOption("tabbycat.clean_names")

    options(tabbycat.clean_names = TRUE)
    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "Cyl",
        "manufacturer",
        "Merc")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = FALSE)
    expected <- tibble::tibble(
        Cyl = c(8, 4, 6, NA),
        n_Merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_Merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "Cyl",
        "manufacturer",
        "Merc")
    expect_equal(observed, expected)

    options(tabbycat.clean_names = restore_option)
})

test_that("cat_contrast returns correct data with a valid only argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = "ignore")
    expect_equal(observed, expected)

    expected_number <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0))

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = "n")
    expect_equal(observed, expected_number)

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = "number")
    expect_equal(observed, expected_number)

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = " number ")
    expect_equal(observed, expected_number)

    expected_percent <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = "p")
    expect_equal(observed, expected_percent)

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = "percent")
    expect_equal(observed, expected_percent)

    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        only = " percent ")
    expect_equal(observed, expected_percent)
})

test_that("cat_contrast returns correct data with a valid other_label argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_not_merc = c(11, 9, 3, 1),
        n_na = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_not_merc = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_na = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        other_label = "not_merc")
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid na_label argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 3, 1),
        n_missing = c(0, 0, 1, 0),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.45833333, 0.37500000, 0.12500000, 0.04166667),
        p_missing = c(0, 0, 1, 0))
    observed <- cat_contrast(
        data,
        "cyl",
        "manufacturer",
        "Merc",
        na_label = "missing")
    expect_equal(observed, expected)
})
