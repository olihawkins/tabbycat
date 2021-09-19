#' Count the frequency of discrete values in the column of a dataframe
#'
#' Calculates the frequency of discrete values in the column of a dataframe
#' containing and returns the counts as a tibble. This function differs from
#' \code{cat_vcount} in that it operates on columns in dataframes rather than
#' directly on vectors, which means it's more useful in pipelines but handles
#' a narrower range of inputs.
#'
#' @param data A data frame containing the vector to summarise.
#' @param cat The name of a column in \code{data} which is a categorical vector
#'   of discrete values for which frequencies will be calculated.
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @param only An optional argument indicating that only one of the frequency
#'   columns should be returned in the results. If \code{only} is either
#'   "number" or "n", only the number column is returned. If \code{only} is
#'   either "percent" or "p", only the percent column is returned. If
#'   \code{only} is any other value, both columns are shown. The default value
#'   is an empty string, which means both columns are shown.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

cat_count <- function(
    data,
    cat,
    na.rm = FALSE,
    only = "") {

    # Check the data argument is not null and is a dataframe
    if (is.null(data) || ! is.data.frame(data)) {
        stop("The data argument is not a dataframe.")
    }

    # Check that data has rows
    if (nrow(data) == 0) {
        stop("The data argument is empty.")
    }

    # Check the cat argument is not null
    if (is.null(cat) || is.na(cat)) {
        stop("The cat argument is null.")
    }

    # Check the cat argument is a column in data
    if (! cat %in% colnames(data)) {
        stop(stringr::str_c("'", cat, "' is not a column in the dataframe."))
    }

    # Check the na.rm argument is valid
    if (is.na(na.rm) || ! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # Remove rows with NAs if na.rm is TRUE
    if (na.rm == TRUE) {
        data <- data %>% dplyr::filter(! is.na(.data[[cat]]))
    }

    count <- data %>%
        dplyr::group_by(.data[[cat]]) %>%
        dplyr::summarise(number = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(percent = .data$number / sum(.data$number)) %>%
        dplyr::arrange(dplyr::desc(.data$number))

    count
}
