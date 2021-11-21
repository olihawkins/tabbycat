#' tabbycat: Tabulate and summarise categorical data
#'
#' Functions for tabulating and summarising categorical variables.
#'
#' @docType package
#' @name tabbycat
#' @importFrom rlang .data
#' @importFrom magrittr %>%
NULL

# Tell R CMD check about new operators
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}
