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
#' @param clean_names A boolean indicating whether the column names of the
#'   results tibble should be cleaned, so that any column names produced from
#'   data are converted to snake_case. The default is TRUE.
#' @param only A string indicating that only one of the frequency columns
#'   should be returned in the results. If \code{only} is either "n" or
#'   "number", only the number column is returned. If \code{only} is either
#'   "p" or "percent", only the percent column is returned. If \code{only} is
#'   any other value, both columns are shown. The default value is an empty
#'   string, which means both columns are shown.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

cat_count <- function(
    data,
    cat,
    na.rm = FALSE,
    clean_names = TRUE,
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

    # Check the clean_names argument is valid
    if (is.na(clean_names) || ! is.logical(clean_names)) {
        stop("Invalid \"clean_names\" argument. Must be either TRUE or FALSE.")
    }

    # Check the only argument is valid
    if (length(only) != 1 || is.na(only) || ! is.character(only)) {
        stop("Invalid \"only\" argument. Must be a single string.")
    }

    # Remove rows with NAs if na.rm is TRUE
    if (na.rm == TRUE) {
        data <- data %>% dplyr::filter(! is.na(.data[[cat]]))
    }

    # Create table
    count <- data %>%
        dplyr::group_by(.data[[cat]]) %>%
        dplyr::summarise(number = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(percent = .data$number / sum(.data$number)) %>%
        dplyr::arrange(dplyr::desc(.data$number))

    # Clean names if clean_names is TRUE
    if (clean_names == TRUE) {
        count <- count %>% janitor::clean_names()
    }

    # Remove columns based on only argument
    if (stringr::str_trim(only) %in% c("n", "number")) {
        count <- count %>% dplyr::select(-.data$percent)
    }

    if (stringr::str_trim(only) %in% c("p", "percent")) {
        count <- count %>% dplyr::select(-.data$number)
    }

    count
}
