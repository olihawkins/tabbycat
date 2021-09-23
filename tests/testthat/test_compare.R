# Test contrast.R

# Setup ----------------------------------------------------------------------

data <- mtcars %>%
    tibble::rownames_to_column("model") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        manufacturer = stringr::str_split(model, " ", simplify = TRUE)[, 1])

data$cyl[1] <- NA

# Tests: cat_compare --------------------------------------------------------

test_that("cat_compare rejects a data argument that is not a dataframe", {

    msg <- "The data argument is not a dataframe."
    expect_error(cat_compare(NULL, "cyl", "vs"), msg)
    expect_error(cat_compare(NA, "cyl", "vs"), msg)
    expect_error(cat_compare(1:10, "cyl", "vs"), msg)
    expect_error(cat_compare(LETTERS[1:10], "cyl", "vs"), msg)
    expect_error(cat_compare(c(TRUE, FALSE), "cyl", "vs"), msg)
    expect_error(cat_compare(list(), "cyl", "vs"), msg)
})

test_that("cat_compare rejects a data argument that has no rows", {

    msg <- "The data argument is empty."
    expect_error(cat_compare(data.frame(), "cyl", "vs"), msg)
})

test_that("cat_compare rejects a dist_cat argument is NULL or NA", {

    msg <- "The dist_cat argument is null."
    expect_error(cat_compare(data, NULL, "manufacturer", "Merc"), msg)
    expect_error(cat_compare(data, NA, "manufacturer", "Merc"), msg)
})

test_that("cat_compare rejects a dist_cat argument is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_compare(data, "notacolumn", "manufacturer", "Merc"), msg)
})

test_that("cat_compare rejects a group_cat argument is NULL or NA", {

    msg <- "The group_cat argument is null."
    expect_error(cat_compare(data, "cyl", NULL, "Merc"), msg)
    expect_error(cat_compare(data, "cyl", NA, "Merc"), msg)
})

test_that("cat_compare rejects a group_cat argument is not a column in the data", {

    msg <- "'notacolumn' is not a column in the dataframe."
    expect_error(cat_compare(data, "cyl", "notacolumn", "Merc"), msg)
})

test_that("cat_compare rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", na.rm = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = list()), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = data.frame()), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", na.rm = ""), msg)
})

test_that("cat_compare rejects invalid clean_names arguments", {

    msg <- "Invalid \"clean_names\" argument. Must be either TRUE or FALSE."
    expect_error(cat_compare(data, "cyl", "vs", clean_names = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = list()), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = data.frame()), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", clean_names = ""), msg)
})

test_that("cat_compare rejects invalid only arguments", {

    msg <- "Invalid \"only\" argument. Must be a single string."
    expect_error(cat_compare(data, "cyl", "vs", only = NULL), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = NA), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = list()), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = data.frame()), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = 1), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = TRUE), msg)
    expect_error(cat_compare(data, "cyl", "vs", only = c("n", "p")), msg)
})

test_that("cat_compare returns correct data with defaults", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(
        data,
        "cyl",
        "vs")
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid na.rm argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        na.rm = FALSE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        cyl = c(4, 6, 8),
        n_0 = c(1, 2, 14),
        n_1 = c(10, 4, 0),
        p_0 = c(0.05882353, 0.11764706, 0.82352941),
        p_1 = c(0.71428571, 0.28571429, 0.0000000))
    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        na.rm = TRUE)
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid clean_names argument", {

    data$Cyl <- data$cyl

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(
        data,
        "Cyl",
        "vs",
        clean_names = TRUE)
    expect_equal(observed, expected)

    expected <- tibble::tibble(
        Cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(0.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(
        data,
        "Cyl",
        "vs",
        clean_names = FALSE)
    expect_equal(observed, expected)
})

test_that("cat_compare returns correct data with a valid only argument", {

    expected <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0))
    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = "ignore")
    expect_equal(observed, expected)

    expected_number <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        n_0 = c(1, 2, 14, 1),
        n_1 = c(10, 4, 0, 0))

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = "n")
    expect_equal(observed, expected_number)

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = "number")
    expect_equal(observed, expected_number)

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = " number ")
    expect_equal(observed, expected_number)

    expected_percent <- tibble::tibble(
        cyl = c(4, 6, 8, NA),
        p_0 = c(0.05555556, 0.11111111, 0.77777778, 0.05555556),
        p_1 = c(.71428571, 0.28571429, 0.0, 0.0))

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = "p")
    expect_equal(observed, expected_percent)

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = "percent")
    expect_equal(observed, expected_percent)

    observed <- cat_compare(
        data,
        "cyl",
        "vs",
        only = " percent ")
    expect_equal(observed, expected_percent)
})
