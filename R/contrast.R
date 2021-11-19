#' Calculate the frequency of discrete values in one categorical variable for
#' each of two mutually exclusive groups within another categorical variable
#'
#' This function shows the distrbution of values within given a categorical
#' variable for one group within another categorical variable, and compares it
#' with the distribution among all observations not in that group. Its purpose
#' is to let you see quickly whether the distribution within that group differs
#' from the distribution for the rest of the observations. The results are
#' sorted in decending order of frequency for the named group i.e. the group
#' named in \code{col_group}.
#'
#' @param data A dataframe containing the two variables of interest.
#' @param row_cat The column name of a categorical variable whose distribution
#'   should be calculated for each exclusive group in \code{col_cat}.
#' @param col_cat The column name of a categorical variable that will be
#'   split into two exclusive groups, one containing observations with a
#'   particular value of that variable, and another containing all other
#'   observations.
#' @param col_group The name of the group within \code{col_cat} that is
#'   used to split the observations into two exclusive groups: those that are
#'   in the group and those that are not in the group.
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
#'   data are converted to snake_case. The default is TRUE, but this can be
#'   changed with \code{options(tabbycat.clean_names = FALSE)}.
#' @param only A string indicating that only one set of frequency columns
#'   should be returned in the results. If \code{only} is either "n" or
#'   "number", only the number columns are returned. If \code{only} is either
#'   "p" or "percent", only the percent columns are returned. If \code{only} is
#'   any other value, both sets of columns are shown. The default value is an
#'   empty string, which means both sets of columns are shown.
#' @param na_label A string indicating the label to use for the columns that
#'   contain data for missing values. The default value is "na", but use this
#'   argument to set a different value if the default value collides with data
#'   in your dataset.
#' @param other_label A string indicating the label to use for the columns that
#'   contain data for observations not in the named group. The default value is
#'   "other", but use this argument to set a different value if the default
#'   value collides with data in your dataset.
#' @return A tibble showing the distribution of \code{row_cat} within each of
#'   the two exclusive groups in \code{col_cat}.
#' @export

cat_contrast <- function(
    data,
    row_cat,
    col_cat,
    col_group,
    na.rm.row = FALSE,
    na.rm.col = FALSE,
    na.rm = NULL,
    only = "",
    clean_names = getOption("tabbycat.clean_names"),
    na_label = getOption("tabbycat.na_label"),
    other_label = getOption("tabbycat.other_label")) {

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
        stop("Invalid \"row_cat\" argument. Must be a character vector of length one.")
    }

    # Check the row_cat argument is a column in data
    if (! row_cat %in% colnames(data)) {
        stop(stringr::str_c("'", row_cat,
            "' is not a column in the dataframe."))
    }

    # Check the col_cat argument is a character vector of length one
    if (! is.character(col_cat) || length(col_cat) != 1) {
        stop("Invalid \"col_cat\" argument. Must be a character vector of length one.")
    }

    # Check the col_cat argument is a column in data
    if (! col_cat %in% colnames(data)) {
        stop(stringr::str_c("'", col_cat,
            "' is not a column in the dataframe."))
    }

    # Check the col_group argument is a valid vector of length one
    if (length(col_group) != 1 || is.na(col_group) || ! is.atomic(col_group)) {
        stop("Invalid \"col_group\" argument. Must be a character vector of length one.")
    }

    # Check that col_group exits in col_cat
    if (! col_group %in% data[[col_cat]]) {
        stop(stringr::str_c(
            "The \"col_group\" '", col_group,
            "' does not exist in the \"col_cat\" '", col_cat, "'."))
    }

    # Check the na.rm.row argument is valid
    if (length(na.rm.row) != 1 || is.na(na.rm.row) || ! is.logical(na.rm.row)) {
        stop("Invalid \"na.rm.row\" argument. Must be either TRUE or FALSE.")
    }

    # Check the na.rm.col argument is valid
    if (length(na.rm.col) != 1 || is.na(na.rm.col) || ! is.logical(na.rm.col)) {
        stop("Invalid \"na.rm.col\" argument. Must be either TRUE or FALSE.")
    }

    # Check the na.rm argument is valid
    if (length(na.rm) > 1 ||
        (! is.null(na.rm) && ! is.logical(na.rm)) ||
        (! is.null(na.rm) && is.na(na.rm))) {
        stop("Invalid \"na.rm\" argument. Must be either NULL, TRUE or FALSE.")
    }

    # Check the only argument is valid
    if (length(only) != 1 || is.na(only) || ! is.character(only)) {
        stop("Invalid \"only\" argument. Must be a character vector of length one.")
    }

    # Check the clean_names argument is valid
    if (length(clean_names) != 1 || is.na(clean_names) || ! is.logical(clean_names)) {
        stop("Invalid \"clean_names\" argument. Must be either TRUE or FALSE.")
    }

    # Check the other_label argument is a character vector of length one
    if (! is.character(other_label) || length(other_label) != 1) {
        stop("Invalid \"other_label\" argument. Must be a character vector of length one.")
    }

    # Check the na_label argument is a character vector of length one
    if (! is.character(na_label) || length(na_label) != 1) {
        stop("Invalid \"na_label\" argument. Must be a character vector of length one.")
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

    in_group_data <- data %>%
        dplyr::filter(.data[[col_cat]] == col_group) %>%
        dplyr::group_by(.data[[row_cat]]) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(p = .data$n / sum(.data$n)) %>%
        dplyr::mutate(group = as.character(col_group)) %>%
        dplyr::select(
            .data$group,
            .data[[row_cat]],
            .data$n,
            .data$p)

    out_group_data <- data %>%
        dplyr::filter(.data[[col_cat]] != col_group) %>%
        dplyr::group_by(.data[[row_cat]]) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(p = .data$n / sum(.data$n)) %>%
        dplyr::mutate(group = other_label) %>%
        dplyr::select(
            .data$group,
            .data[[row_cat]],
            .data$n,
            .data$p)

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
            in_group_data,
            out_group_data,
            na_data) %>%
        tidyr::pivot_wider(
            id_cols = {{row_cat}},
            names_from = .data$group,
            values_from = c(.data$n, .data$p)) %>%
        dplyr::mutate(dplyr::across(-1, ~tidyr::replace_na(.x, 0))) %>%
        dplyr::arrange(dplyr::desc(.data[[stringr::str_c("n_", col_group)]]))

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
