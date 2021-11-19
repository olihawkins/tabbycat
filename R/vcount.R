#' Count the frequency of discrete values in a categorical vector
#'
#' This function differs from \code{cat_count} in that it operates directly on
#' vectors. rather than on columns in dataframes, which means it is less useful
#' in pipelines but can handle a wider range of inputs. The results are sorted
#' in decending order of frequency.
#'
#' @param cat A categorical vector of for which frequencies will be calculated.
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @param only A string indicating that only one of the frequency columns
#'   should be returned in the results. If \code{only} is either "n" or
#'   "number", only the number column is returned. If \code{only} is either
#'   "p" or "percent", only the percent column is returned. If \code{only} is
#'   any other value, both columns are shown. The default value is an empty
#'   string, which means both columns are shown.
#' @param clean_names A boolean indicating whether the column names of the
#'   results tibble should be cleaned, so that any column names produced from
#'   data are converted to snake_case. The default is TRUE, but this can be
#'   changed with \code{options(tabbycat.clean_names = FALSE)}.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

cat_vcount <- function(
    cat,
    na.rm = FALSE,
    only = "",
    clean_names = getOption("tabbycat.clean_names")) {

    # Check the cat argument is not null and is a vector
    if (is.null(cat) || ! is.atomic(cat)) {
        stop("The \"cat\" argument is not a vector.")
    }

    # Check the cat argument is not an empty factor
    if (length(cat) == 0) {
        stop("The \"cat\" argument is empty.")
    }

    # Check the na.rm argument is valid
    if (length(na.rm) != 1 || is.na(na.rm) || ! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # Check the only argument is valid
    if (length(only) != 1 || is.na(only) || ! is.character(only)) {
        stop("Invalid \"only\" argument. Must be a character vector of length one.")
    }

    # Check the clean_names argument is valid
    if (length(clean_names) != 1 || is.na(clean_names) || ! is.logical(clean_names)) {
        stop("Invalid \"clean_names\" argument. Must be either TRUE or FALSE.")
    }

    # Get the variable name of the cat argument
    obj_name <- deparse(substitute(cat))

    # If the name is a dataframe and a column extract the column name
    if(stringr::str_detect(obj_name, "\\$")) {
        obj_name <- stringr::str_split_fixed(obj_name, "\\$", 2)[1, 2]
    }

    # Set the name
    name <- obj_name

    # Set option for handling NAs
    use_na <- ifelse(na.rm, "no", "ifany")

    # Create the results dataframe and return
    count <- tibble::as_tibble(
        table(
            cat,
            useNA = use_na),
            .name_repair = ~ c(name, "number")) %>%
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
