% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcount.R
\name{cat_vcount}
\alias{cat_vcount}
\title{Count the frequency of discrete values in a categorical vector}
\usage{
cat_vcount(
  cat,
  na.rm = FALSE,
  only = "",
  clean_names = getOption("tabbycat.clean_names")
)
}
\arguments{
\item{cat}{A categorical vector for which frequencies will be calculated.}

\item{na.rm}{A boolean indicating whether to exclude NAs from the results.
The default is FALSE.}

\item{only}{A string indicating that only one of the frequency columns
should be returned in the results. If \code{only} is either "n" or
"number", only the number column is returned. If \code{only} is either
"p" or "percent", only the percent column is returned. If \code{only} is
any other value, both columns are shown. The default value is an empty
string, which means both columns are shown.}

\item{clean_names}{A boolean indicating whether the column names of the
results tibble should be cleaned, so that any column names produced from
data are converted to snake_case. The default is TRUE, but this can be
changed with \code{options(tabbycat.clean_names = FALSE)}.}
}
\value{
A tibble showing the frequency of each value in \code{cat}.
}
\description{
This function differs from \code{cat_count} in that it operates directly on
vectors, rather than on columns in dataframes, which means it is less useful
in pipelines but can handle a wider range of inputs. The results are sorted
in descending order of frequency.
}
