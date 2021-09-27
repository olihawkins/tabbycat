#' Summarise the values of a numerical variable for each group within a
#' categorical variable and return the results as a tibble
#'
#' The results are sorted on the values of the categorical variable i.e.
#' the variable specified with \code{cat}. If this variable is a character
#' vector it will be sorted alphabetically. If it is a factor it will be
#' sorted in the order of its levels.
#'
#' @param data A dataframe containing the vector to summarise.
#' @param cat The name of a column in \code{data} which is a categorical vector
#'   of discrete values for which summaries will be calculated.
#' @param num The name of a column in \code{data} which is a numerical vector
#'   that will be summarised for each group.
#' @param na.rm A boolean indicating whether to exclude NAs from the row
#'   results. Note that NAs are **always** ignored in calculating the summary
#'   statistics for \code{num} shown in each row, and the number of NAs that
#'   exist in \code{num} for each group in \code{cat} is shown in the
#'   \code{na} column of the results table. This argument controls whether a
#'   row of summary statistics is shown for observations that are NA in
#'   \code{cat}. The default is FALSE.
#' @param clean_names A boolean indicating whether the column names of the
#'   results tibble should be cleaned, so that any column names produced from
#'   data are converted to snake_case. The default is TRUE, but this can be
#'   changed with \code{options(tabbycat.clean_names = FALSE)}.
#' @export

cat_summarise <- function(
    data,
    cat,
    num,
    na.rm = FALSE,
    clean_names = getOption("tabbycat.clean_names")) {

    # Check the data argument is not null and is a dataframe
    if (is.null(data) || ! is.data.frame(data)) {
        stop("The \"data\" argument is not a dataframe.")
    }

    # Check that data has rows
    if (nrow(data) == 0) {
        stop("The \"data\" argument is empty.")
    }

    # Check the cat argument is a character vector of length one
    if (! is.character(cat) || length(cat) != 1) {
        stop("Invalid \"cat\" argument. Must be a character vector of length one.")
    }

    # Check the cat argument is a column in data
    if (! cat %in% colnames(data)) {
        stop(stringr::str_c("'", cat, "' is not a column in the dataframe."))
    }

    # Check the num argument is a character vector of length one
    if (! is.character(num) || length(num) != 1) {
        stop("Invalid \"num\" argument. Must be a character vector of length one.")
    }

    # Check the num argument is a column in data
    if (! num %in% colnames(data)) {
        stop(stringr::str_c("'", num, "' is not a column in the dataframe."))
    }

    # Check the num argument is numeric
    if (! is.numeric(data[[num]])) {
        stop(stringr::str_c("The num argument is not a numeric column."))
    }

    # Check the na.rm argument is valid
    if (length(na.rm) != 1 || is.na(na.rm) || ! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # Check the clean_names argument is valid
    if (length(clean_names) != 1 || is.na(clean_names) || ! is.logical(clean_names)) {
        stop("Invalid \"clean_names\" argument. Must be either TRUE or FALSE.")
    }

    # Remove rows with NAs if na.rm is TRUE
    if (na.rm == TRUE) {
        data <- data %>% dplyr::filter(! is.na(.data[[cat]]))
    }

    # Create table
    summary <- data %>%
        dplyr::group_by(.data[[cat]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            na = sum(is.na(.data[[num]])),
            mean = safe_mean(.data[[num]], na.rm = TRUE),
            sd = stats::sd(.data[[num]], na.rm = TRUE),
            min = safe_min(.data[[num]], na.rm = TRUE),
            lq = unname(stats::quantile(.data[[num]], 0.25, na.rm = TRUE)),
            med = stats::median(.data[[num]], na.rm = TRUE),
            uq = unname(stats::quantile(.data[[num]], 0.75, na.rm = TRUE)),
            max = safe_max(.data[[num]], na.rm = TRUE),
            .groups = "drop") %>%
        dplyr::arrange(.data[[cat]])

    # Clean names if clean_names is TRUE
    if (clean_names == TRUE) {
        summary <- summary %>% janitor::clean_names()
    }

    summary
}


#' Calculate \code{mean} but return NA rather than NaN when values are missing
#'
#' This function is a drop-in replacement for \code{mean}, which is used in cat
#' summarise. It returns NA rather than NaN when all values are NA.
#'
#' @param x A numerical vector.
#' @param na.rm A boolean indicating whether to remove NAs.
#' @keywords internal

safe_mean <- function(x, na.rm = FALSE) {
    result <- mean(x, na.rm = na.rm)
    if (is.na(result)) return(NA)
    result
}

#' Calculate \code{min} but suppress the warning when all values are missing
#'
#' This function is a drop-in replacement for \code{min}, which is used in cat
#' summarise. It suppresses the warning when all values are NA and na.rm is
#' TRUE, and returns NA instean of Inf.
#'
#' @param x A numerical vector.
#' @param na.rm A boolean indicating whether to remove NAs.
#' @keywords internal

safe_min <- function(x, na.rm = FALSE) {
    result <- NA
    tryCatch(
        result <- min(x, na.rm = na.rm),
        warning = function(c) c)
    result
}


#' Calculate \code{max} but suppress the warning when all values are missing
#'
#' This function is a drop-in replacement for \code{max}, which is used in cat
#' summarise. It suppresses the warning when all values are NA and na.rm is
#' TRUE, and returns NA instean of -Inf.
#'
#' @param x A numerical vector.
#' @param na.rm A boolean indicating whether to remove NAs.
#' @keywords internal

safe_max <- function(x, na.rm = FALSE) {
    result <- NA
    tryCatch(
        result <- max(x, na.rm = na.rm),
        warning = function(c) c)
    result
}
