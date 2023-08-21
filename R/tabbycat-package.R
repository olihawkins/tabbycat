#' tabbycat: Tabulate and summarise categorical data
#'
#' Functions for tabulating and summarising categorical variables.
#'
#' @name tabbycat
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @keywords internal
"_PACKAGE"

# Tell R CMD check about new operators
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}
