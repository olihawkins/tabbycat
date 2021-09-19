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

    msg <- "The data argument is not a dataframe."
    expect_error(cat_contrast(NULL, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(NA, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(1:10, "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(LETTERS[1:10], "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(c(TRUE, FALSE), "cyl", "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(list(), "cyl", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a data argument that has no rows", {

    msg <- "The data argument is empty."
    expect_error(cat_contrast(data.frame(), "cyl", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a dist_cat argument is NULL or NA", {

    msg <- "The dist_cat argument is null."
    expect_error(cat_contrast(data, NULL, "manufacturer", "Merc"), msg)
    expect_error(cat_contrast(data, NA, "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a dist_cat argument is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_contrast(data, "notacolumn", "manufacturer", "Merc"), msg)
})

test_that("cat_contrast rejects a group_cat argument is NULL or NA", {

    msg <- "The group_cat argument is null."
    expect_error(cat_contrast(data, "cyl", NULL, "Merc"), msg)
    expect_error(cat_contrast(data, "cyl", NA, "Merc"), msg)
})

test_that("cat_contrast rejects a group_cat argument is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_contrast(data, "cyl", "notacolumn", "Merc"), msg)
})

test_that("cat_contrast rejects a group_name argument is NULL or NA", {

    msg <- "The group_name argument is null."
    expect_error(cat_contrast(data, "cyl", "manufacturer", NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", NA), msg)
})

test_that("cat_contrast rejects a group_name argument that does not exist in group_cat", {

    msg <- "The group_name 'notagroup' does not exist in the group_cat 'manufacturer'."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "notagroup"), msg)
})

test_that("cat_contrast rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = NULL), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = NA), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = list()), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = data.frame()), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = 1), msg)
    expect_error(cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = ""), msg)
})

test_that("cat_contrast returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 4, 1),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.44, 0.36, 0.16, 0.04))
    observed <- cat_contrast(data,  "cyl", "manufacturer", "Merc")
    expect_equal(observed, expected)
})

test_that("cat_contrast returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(8, 4, 6, NA),
        n_merc = c(3, 2, 2, 0),
        n_other = c(11, 9, 4, 1),
        p_merc = c(0.42857143, 0.28571429, 0.28571429, 0.0),
        p_other = c(0.44, 0.36, 0.16, 0.04))
    observed <- cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(8, 4, 6),
        n_merc = c(3, 2, 2),
        n_other = c(11, 9, 4),
        p_merc = c(0.42857143, 0.28571429, 0.28571429),
        p_other = c(0.45833333, 0.3750000, 0.166666667))
    observed <- cat_contrast(data, "cyl", "manufacturer", "Merc", na.rm = TRUE)
    expect_equal(observed, expected)
})
