#' Summarise the values of a numerical variable for each group within a
#' categorical variable and return the results as a tibble
#'
#' The results are sorted on the values of the categorical variable i.e.
#' the variable specified with \code{cat}. If this variable is a character
#' vector it will be sorted alphabetically, and if it is a factor it will be
#' sorted in the order of its levels.
#'
#' @param data A data frame containing the vector to summarise.
#' @param cat The name of a column in \code{data} which is a categorical vector
#'   of discrete values for which frequencies will be calculated.
#' @param num The name of a column in \code{data} which is a numerical vector
#'   that will be summarised for each group.
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @param clean_names A boolean indicating whether the column names of the
#'   results tibble should be cleaned, so that any column names produced from
#'   data are converted to snake_case. The default is TRUE.
#' @export

cat_summarise <- function(
    data,
    cat,
    num,
    na.rm = FALSE,
    clean_names = TRUE) {

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
        stop("The \"cat\" argument is not a character vector of length one.")
    }

    # Check the cat argument is a column in data
    if (! cat %in% colnames(data)) {
        stop(stringr::str_c("'", cat, "' is not a column in the dataframe."))
    }

    # Check the num argument is a character vector of length one
    if (! is.character(num) || length(num) != 1) {
        stop("The \"num\" argument is not a character vector of length one.")
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

        if (any(is.na(data[[cat]])) || any(is.null(data[[cat]]))) {
            message(stringr::str_c(
                "Rows with missing values in column '", cat,
                "' have been removed."))
        }

        if (any(is.na(data[[num]])) || any(is.null(data[[num]]))) {
            message(stringr::str_c(
                "Rows with missing values in column '", num,
                "' have been removed."))
        }

        data <- data %>%
            dplyr::filter(! is.na(.data[[cat]])) %>%
            dplyr::filter(! is.na(.data[[num]]))
    }

    # Create table
    summary <- data %>%
        dplyr::group_by(.data[[cat]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            na = sum(is.na(.data[[num]])),
            mean = mean(.data[[num]], na.rm = TRUE),
            sd = stats::sd(.data[[num]], na.rm = TRUE),
            min = min(.data[[num]], na.rm = TRUE),
            lq = unname(stats::quantile(.data[[num]], 0.25, na.rm = TRUE)),
            med = stats::median(.data[[num]], na.rm = TRUE),
            uq = unname(stats::quantile(.data[[num]], 0.75, na.rm = TRUE)),
            max = max(.data[[num]], na.rm = TRUE),
            .groups = "drop") %>%
        dplyr::arrange(.data[[cat]])

    # Clean names if clean_names is TRUE
    if (clean_names == TRUE) {
        summary <- summary %>% janitor::clean_names()
    }

    summary
}
