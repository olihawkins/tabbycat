#' Calculate the frequency of discrete values in one categorical variable for
#' each of two mutually exclusive groups within another categorical variable
#' and returns the results as a tibble
#'
#' This function shows the distrbution of values within given a categorical
#' variable for one group within another categorical variable, and compares it
#' with the distribution among all observations not in that group. Its purpose
#' is to let you see quickly whether the distribution within that group differs
#' from the distribution for the rest of the observations.
#'
#' @param data A dataframe containing the two variables of interest.
#' @param dist_cat The name of a categorical variable whose distribution should
#'   be calculated for each group.
#' @param group_cat The name of a categorical variable that will be split
#'   into two exclusive groups, one belonging to observations with a
#'   particular value of that variable, and another containing all other
#'   observations.
#' @param group_name The name of the group within \code{group_cat} that is
#'   used to split the observations into two exclusive groups: those that are
#'   in the group and those that are not in the group.
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @param only An optional argument indicating that only one set of frequency
#'   columns should be returned in the results. If \code{only} is either "n" or
#'   "number", only the number columns are returned. If \code{only} is either
#'   "p" or "percent", only the percent columns are returned. If \code{only} is
#'   any other value, both sets of columns are shown. The default value is an
#'   empty string, which means both sets of columns are shown.
#' @return A tibble showing the distribution of \code{dist_cat} within each of
#'   the two exclusive groups in \code{group_cat}.
#' @export

cat_contrast <- function(
    data,
    dist_cat,
    group_cat,
    group_name,
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

    # Check the dist_cat argument is not null
    if (is.null(dist_cat) || is.na(dist_cat)) {
        stop("The dist_cat argument is null.")
    }

    # Check the dist_cat argument is a column in data
    if (! dist_cat %in% colnames(data)) {
        stop(stringr::str_c("'", dist_cat,
            "' is not a column in the dataframe."))
    }

    # Check the group_cat argument is not null
    if (is.null(group_cat) || is.na(group_cat)) {
        stop("The group_cat argument is null.")
    }

    # Check the group_cat argument is a column in data
    if (! group_cat %in% colnames(data)) {
        stop(stringr::str_c("'", group_cat,
            "' is not a column in the dataframe."))
    }

    # Check the group_name argument is not null
    if (is.null(group_name) || is.na(group_name)) {
        stop("The group_name argument is null.")
    }

    # Check that group_name exits in group_cat
    if (! group_name %in% data[[group_cat]]) {
        stop(stringr::str_c(
            "The group_name '", group_name,
            "' does not exist in the group_cat '", group_cat, "'."))
    }

    # Check the na.rm argument is valid
    if (is.na(na.rm) || ! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # Check the only argument is valid
    if (length(only) != 1 || is.na(only) || ! is.character(only)) {
        stop("Invalid \"only\" argument. Must be a single string.")
    }

    # Remove rows with NAs if na.rm is TRUE
    if (na.rm == TRUE) {
        data <- data %>% dplyr::filter(! is.na(.data[[dist_cat]]))
    }

    in_group_data <- data %>%
        dplyr::filter(.data[[group_cat]] == group_name) %>%
        dplyr::group_by(.data[[dist_cat]]) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(p = .data$n / sum(.data$n)) %>%
        dplyr::mutate(group = as.character(group_name)) %>%
        dplyr::select(
            .data$group,
            .data[[dist_cat]],
            .data$n,
            .data$p)

    out_group_data <- data %>%
        dplyr::filter(.data[[group_cat]] != group_name) %>%
        dplyr::group_by(.data[[dist_cat]]) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(p = .data$n / sum(.data$n)) %>%
        dplyr::mutate(group = "Other") %>%
        dplyr::select(
            .data$group,
            .data[[dist_cat]],
            .data$n,
            .data$p)

    comparison <- dplyr::bind_rows(in_group_data, out_group_data) %>%
        tidyr::pivot_wider(
            id_cols = {{dist_cat}},
            names_from = .data$group,
            values_from = c(.data$n, .data$p)) %>%
        dplyr::mutate(dplyr::across(-1, ~tidyr::replace_na(.x, 0))) %>%
        dplyr::arrange(dplyr::desc(.data[[stringr::str_c("n_", group_name)]])) %>%
        janitor::clean_names()

    # Remove columns based on only argument
    if (stringr::str_trim(only) %in% c("n", "number")) {
        comparison <- comparison %>% dplyr::select(-dplyr::starts_with("p_"))
    }

    if (stringr::str_trim(only) %in% c("p", "percent")) {
        comparison <- comparison %>% dplyr::select(-dplyr::starts_with("n_"))
    }

    comparison
}
