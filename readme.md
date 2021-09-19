# tabbycat

A small library of functions for tabulating and summarising categorical variables in dataframes. The functions share a common interface which takes a dataframe as the first argument so they fit nicely into pipelines.

**This package is in development and is not ready for general use.**

## Installation

Install from GitHub using remotes.

``` r
install.packages("remotes")
remotes::install_github("olihawkins/tabbycat")
```

## cat_count

`cat_count` calculates the frequency of discrete values in the column of a dataframe and returns the counts as a tibble.

``` r
# Load tidyverse for the mpg dataset
library(tidyverse) 
library(tabbycat)

cat_count(mpg, "class")
# # A tibble: 7 × 3
#   class      number percent
#   <chr>       <int>   <dbl>
# 1 suv            62  0.265 
# 2 compact        47  0.201 
# 3 midsize        41  0.175 
# 4 subcompact     35  0.150 
# 5 pickup         33  0.141 
# 6 minivan        11  0.0470
# 7 2seater         5  0.0214
```

## cat_contrast

`cat_contrast` calculates the frequency of discrete values in one categorical variable for each of two mutually exclusive groups within another categorical variable. This lets you see if the distribution of a variable within a particular group differs from the distribution in the rest of the dataset.

``` r
# Load tidyverse for the mpg dataset
library(tidyverse)
library(tabbycat)

cat_contrast(mpg, "class", "manufacturer", "toyota")
# # A tibble: 7 × 5
#   class      n_toyota n_other p_toyota p_other
#   <chr>         <dbl>   <dbl>    <dbl>   <dbl>
# 1 compact          12      35    0.353   0.175
# 2 suv               8      54    0.235   0.27 
# 3 midsize           7      34    0.206   0.17 
# 4 pickup            7      26    0.206   0.13 
# 5 2seater           0       5    0       0.025
# 6 minivan           0      11    0       0.055
# 7 subcompact        0      35    0       0.175
```



