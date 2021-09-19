# Test vcount.R

# Setup ----------------------------------------------------------------------

cat <- c("a", "b", "b", "c", "c", "c", "d", "d", "d", "d")

# Tests: cat_vcount --------------------------------------------------------

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

test_that("cat_vcount rejects invalid by arguments", {

    msg <- "Invalid \"by\" argument. Must be either \"number\" or \"category\"."
    expect_error(cat_vcount(cat, by = NULL), msg)
    expect_error(cat_vcount(cat, by = NA), msg)
    expect_error(cat_vcount(cat, by = list()), msg)
    expect_error(cat_vcount(cat, by = data.frame()), msg)
    expect_error(cat_vcount(cat, by = FALSE), msg)
    expect_error(cat_vcount(cat, by = 1), msg)
    expect_error(cat_vcount(cat, by = ""), msg)
})

test_that("cat_vcount rejects invalid order arguments", {

    msg <- "Invalid \"order\" argument. Must be either \"asc\" or \"desc\"."
    expect_error(cat_vcount(cat, order = NULL), msg)
    expect_error(cat_vcount(cat, order = NA), msg)
    expect_error(cat_vcount(cat, order = list()), msg)
    expect_error(cat_vcount(cat, order = data.frame()), msg)
    expect_error(cat_vcount(cat, order = FALSE), msg)
    expect_error(cat_vcount(cat, order = 1), msg)
    expect_error(cat_vcount(cat, order = ""), msg)
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

test_that("cat_vcount rejects invalid name arguments", {

    msg <- "Invalid \"name\" argument. Must be a string."
    expect_error(cat_vcount(cat, name = NA), msg)
    expect_error(cat_vcount(cat, name = list()), msg)
    expect_error(cat_vcount(cat, name = data.frame()), msg)
})

test_that("cat_vcount returns correct data with defaults", {

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat)
    expect_equal(output, correct)
})

test_that("cat_vcount returns correct data with a given name argument", {

    correct <- tibble::tibble(
        checkname = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, name = "checkname")
    expect_equal(output, correct)
})

test_that("cat_vcount returns correct data with valid by arguments", {

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "number")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[1:4],
        number = 1:4,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "category")
    expect_equal(output, correct)
})

test_that("cat_vcount returns correct data with valid order arguments", {

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[1:4],
        number = 1:4,
        percent = number / sum(number))
    output <- cat_vcount(cat, order = "asc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "number", order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[1:4],
        number = 1:4,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "number", order = "asc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[4:1],
        number = 4:1,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "category", order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        cat = letters[1:4],
        number = 1:4,
        percent = number / sum(number))
    output <- cat_vcount(cat, by = "category", order = "asc")
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
