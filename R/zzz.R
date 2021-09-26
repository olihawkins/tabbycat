.onLoad <- function(libname, pkgname) {

    # Set default options if options have not already been set
    op <- options()

    op_tabbycat <- list(
        tabbycat.clean_names = TRUE,
        tabbycat.na_label = "na",
        tabbycat.other_label = "other")

    to_set <- !(names(op_tabbycat) %in% names(op))
    if (any(to_set)) options(op_tabbycat[to_set])
    invisible()
}
