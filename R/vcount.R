#' Count the frequency of discrete values in a categorical vector
#'
#' Calculates the frequency of discrete values in a categorical vector and
#' returns the counts as a tibble. This function differs from \code{cat_count}
#' in that it operates directly on vectors. rather than on columns in
#' dataframes, which means it's less useful in pipelines but can handle a
#' wider range of inputs.
#'
#' @param cat A categorical vector of discrete values for which
#'   frequencies will be calculated.
#' @param name The column name to use for the column of categories in the
#'   results as a string. By default, the variable name of the input vector is
#'   used.
#' @param by The column name by which to sort the results as a string. Must be
#'   one of either "n" or "cat". The default is "count".
#' @param order The order in which to sort the results as a string. Must be
#'   one of either "asc" or "desc". The default depends on the value of the
#'   \code{by} argument. If \code{by} is "count" the default \code{order} is
#'   "desc". If \code{by} is "category" the default \code{order} is "asc".
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

cat_vcount <- function(
    cat,
    name = NULL,
    by = "number",
    order = ifelse(by == "category", "asc", "desc"),
    na.rm = FALSE) {

    # Get the variable name of the cat argument
    obj_name <- deparse(substitute(cat))

    # Check the cat argument is not null and is a vector
    if (is.null(cat) || ! is.atomic(cat)) {
        stop("The cat argument is not a vector.")
    }

    # Check the cat argument is not an empty factor
    if (length(cat) == 0) {
        stop("The cat argument is empty.")
    }

    # Check the sort arguments are valid
    if (length(by) != 1 || ! by %in% c("number", "category")) {
        stop("Invalid \"by\" argument. Must be either \"number\" or \"category\".")
    }

    if (length(order) != 1 || ! order %in% c("asc", "desc")) {
        stop("Invalid \"order\" argument. Must be either \"asc\" or \"desc\".")
    }

    # Check the na.rm argument is valid
    if (is.na(na.rm) || ! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # If name is not provided use the variable name
    if (is.null(name)) {
        # If the name is a dataframe and a column extract the column name
        if(stringr::str_detect(obj_name, "\\$")) {
            obj_name <- stringr::str_split_fixed(obj_name, "\\$", 2)[1, 2]
        }
        name <- obj_name
    }

    # Check the name argument is valid
    if (length(name) != 1 || is.na(name) || ! is.atomic(cat)) {
        stop("Invalid \"name\" argument. Must be a string.")
    }

    # Set the sort properties for the call to arrange
    by_col <- ifelse(by == "n", "n", name)
    order_func <- ifelse(order == "desc", dplyr::desc, function(d){d})

    # Set option for handling NAs
    use_na <- ifelse(na.rm, "no", "ifany")

    # Create the results dataframe and return
    tibble::as_tibble(
        table(
            cat,
            useNA = use_na),
            .name_repair = ~ c(name, "number")) |>
        dplyr::arrange(order_func(.data[[by_col]])) |>
        dplyr::mutate(percent = .data$number / sum(.data$number))
}
