#' tabbycat: Tabulate and summarise categorical data
#'
#' A small library of functions for exploring, tabulating, and summarising
#' categorical variables.
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
