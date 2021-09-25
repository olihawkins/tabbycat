#' Calculate the frequency of discrete values in one categorical variable for
#' each group within another categorical variable and return the results as a
#' tibble
#'
#' This function crosstabulates the frequencies of one categorical variable
#' within the groups of another.
#'
#' The results are sorted on the values of the variable whose distribution is
#' shown in each column i.e. the variable specified with \code{row_cat}. If
#' this variable is a character vector it will be sorted alphabetically. If
#' it is a factor it will be sorted in the order of its levels.
#'
#' @param data A dataframe containing the two variables of interest.
#' @param row_cat The column name of a categorical variable whose distribution
#'   will be calculated for each group in \code{col_cat}.
#' @param col_cat The column name of a categorical variable which will be
#'   split into groups and the distrubtion of \code{row_cat} calulated
#'   for each group.
#' @param na.rm.row A boolean indicating whether to exclude NAs from the row
#'   results. The default is FALSE.
#' @param na.rm.col A boolean indicating whether to exclude NAs from the column
#'   results. The default is FALSE.
#' @param na.rm A boolean indicating whether to exclude NAs from both row and
#'   column results. This argument is provided as a convenience. It allows you
#'   to set \code{na.rm.row} and \code{na.rm.col} to the same value without
#'   having to specify them separately. If the value of \code{na.rm} is NULL,
#'   the argument is ignored. If it is not NULL it takes precendence.
#'   default is NULL.
#' @param clean_names A boolean indicating whether the column names of the
#'   results tibble should be cleaned, so that any column names produced from
#'   data are converted to snake_case. The default is TRUE.
#' @param only A string indicating that only one set of frequency columns
#'   should be returned in the results. If \code{only} is either "n" or
#'   "number", only the number columns are returned. If \code{only} is either
#'   "p" or "percent", only the percent columns are returned. If \code{only} is
#'   any other value, both sets of columns are shown. The default value is an
#'   empty string, which means both sets of columns are shown.
#' @param na_label A string indicating the label to use for the columns that
#'   contain data for missing values. The default value is "na", but use this
#'   argument to set a different value if the default  value collides with data
#'   in your dataset.
#' @return A tibble showing the distribution of \code{row_cat} within each of
#'   the two exclusive groups in \code{col_cat}.
#' @export

cat_compare <- function(
    data,
    row_cat,
    col_cat,
    na.rm.row = FALSE,
    na.rm.col = FALSE,
    na.rm = NULL,
    clean_names = TRUE,
    only = "",
    na_label = "na") {

    # Check the data argument is not null and is a dataframe
    if (is.null(data) || ! is.data.frame(data)) {
        stop("The \"data\" argument is not a dataframe.")
    }

    # Check that data has rows
    if (nrow(data) == 0) {
        stop("The \"data\" argument is empty.")
    }

    # Check the row_cat argument is a character vector of length one
    if (! is.character(row_cat) || length(row_cat) != 1) {
        stop("The \"row_cat\" argument is not a character vector of length one.")
    }

    # Check the row_cat argument is a column in data
    if (! row_cat %in% colnames(data)) {
        stop(stringr::str_c("'", row_cat,
            "' is not a column in the dataframe."))
    }

    # Check the col_cat argument is a character vector of length one
    if (! is.character(col_cat) || length(col_cat) != 1) {
        stop("The \"col_cat\" argument is not a character vector of length one.")
    }

    # Check the col_cat argument is a column in data
    if (! col_cat %in% colnames(data)) {
        stop(stringr::str_c("'", col_cat,
            "' is not a column in the dataframe."))
    }

    # Check the na.rm.row argument is valid
    if (length(na.rm.row) != 1 || is.na(na.rm.row) || ! is.logical(na.rm.row)) {
        stop("Invalid \"na.rm.row\" argument. Must be either TRUE or FALSE.")
    }

    # Check the na.rm.col argument is valid
    if (length(na.rm.col) != 1 || is.na(na.rm.col) || ! is.logical(na.rm.col)) {
        stop("Invalid \"na.rm.col\" argument. Must be either TRUE or FALSE.")
    }

    # Check the clean_names argument is valid
    if (length(clean_names) != 1 || is.na(clean_names) || ! is.logical(clean_names)) {
        stop("Invalid \"clean_names\" argument. Must be either TRUE or FALSE.")
    }

    # Check the only argument is valid
    if (length(only) != 1 || is.na(only) || ! is.character(only)) {
        stop("Invalid \"only\" argument. Must be a single string.")
    }

    # Set both na.rm.row and na.rm.col to the value of na.rm if specified
    if (! is.null(na.rm)) {
        na.rm.row <- na.rm
        na.rm.col <- na.rm
    }

    # Remove rows with NAs if na.rm.row is TRUE
    if (na.rm.row == TRUE) {
        data <- data %>% dplyr::filter(! is.na(.data[[row_cat]]))
    }

    group_names <- sort(unique(data[[col_cat]]))

    comparison_data <- purrr::map_dfr(group_names, function(group_name) {

        data %>%
            dplyr::filter(.data[[col_cat]] == group_name) %>%
            dplyr::group_by(.data[[row_cat]]) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(p = .data$n / sum(.data$n)) %>%
            dplyr::mutate(group = as.character(group_name)) %>%
            dplyr::select(
                .data$group,
                .data[[row_cat]],
                .data$n,
                .data$p)
    })

    if (na.rm.col == FALSE) {

        na_data <- data %>%
            dplyr::filter(is.na(.data[[col_cat]])) %>%
            dplyr::group_by(.data[[row_cat]]) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(p = .data$n / sum(.data$n)) %>%
            dplyr::mutate(group = na_label) %>%
            dplyr::select(
                .data$group,
                .data[[row_cat]],
                .data$n,
                .data$p)

    } else {

        na_data <- tibble::tibble()
    }

    comparison <- dplyr::bind_rows(
            comparison_data,
            na_data) %>%
        tidyr::pivot_wider(
            id_cols = {{row_cat}},
            names_from = .data$group,
            values_from = c(.data$n, .data$p)) %>%
        dplyr::mutate(dplyr::across(-1, ~tidyr::replace_na(.x, 0))) %>%
        dplyr::arrange(.data[[row_cat]])

    # Clean names if clean_names is TRUE
    if (clean_names == TRUE) {
        comparison <- comparison %>% janitor::clean_names()
    }

    # Remove columns based on only argument
    if (stringr::str_trim(only) %in% c("n", "number")) {
        comparison <- comparison %>% dplyr::select(-dplyr::starts_with("p_"))
    }

    if (stringr::str_trim(only) %in% c("p", "percent")) {
        comparison <- comparison %>% dplyr::select(-dplyr::starts_with("n_"))
    }

    comparison
}
