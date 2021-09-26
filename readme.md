# tabbycat

`tabbycat` is a small library of functions for tabulating and summarising categorical variables. Most of the functions are designed to work with dataframes, and use the tidyverse idiom of taking the dataframe as the first argument so they fit easily into pipelines. Equivalent functions that operate directly on vectors are also provided where it makes sense.

**This package is in development and is almost a beta, but not quite.**

## Contents

* [Installation](#installation)
* [Counting functions](#counting-functions)
* [Comparison functions](#comparison-functions)
* [Summarising functions](#summarising-functions)
* [Other API features](#other-api-features)

## List of functions

* [`cat_count`](#cat_count)
* [`cat_vcount`](#cat_vcount)
* [`cat_compare`](#cat_compare)
* [`cat_contrast`](#cat_contrast)
* [`cat_summarise`](#cat_summarise)

## 1. Installation

Install from GitHub using remotes.

```r
install.packages("remotes")
remotes::install_github("olihawkins/tabbycat")
```

## 2. Counting functions

### `cat_count`

`cat_count` calculates the frequency of discrete values in the column of a dataframe and returns the counts and percentages as a tibble. This function operates on columns in dataframes, but an equivalent function called `cat_vcount` provides the same functionality for vectors. Call the function with a dataframe and the name of the column to count.

```r
# Load tabbycat and the mpg dataset
library(tabbycat)
mpg <- ggplot2::mpg

cat_count(mpg, "class")
#   A tibble: 7 × 3
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

### `cat_vcount`

`cat_vcount` is equivalent to `cat_count` but works directly on vectors: it calculates the frequency of discrete values in a vector and returns the counts and percentages as a tibble. `cat_vcount` can handle a wider range of inputs than `cat_count` but it does not fit as easily into pipelines. Call the function with a vector to count.

```r
# Load tabbycat and the mpg dataset
library(tabbycat)
mpg <- ggplot2::mpg

cat_vcount(mpg$class)
#   A tibble: 7 × 3
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

### 2.1. NA handling for counting functions

By default, the frequency of NAs is included in the results, but you can remove this by setting the `na.rm` argument to `TRUE`. This means the percentages are caclulated excluding NAs (i.e. based on the the counts shown in the table).

``` r
# Set the class of the first observation to NA
mpg[1, ]$class <- NA

# Call cat_summarise with defaults
cat_count(mpg, "class")
#   A tibble: 8 × 3
#   class      number percent
#   <chr>       <int>   <dbl>
# 1 suv            62 0.265  
# 2 compact        46 0.197  
# 3 midsize        41 0.175  
# 4 subcompact     35 0.150  
# 5 pickup         33 0.141  
# 6 minivan        11 0.0470 
# 7 2seater         5 0.0214 
# 8 NA              1 0.00427

# Call cat_summarise with na.rm set to TRUE
cat_count(mpg, "class", na.rm = TRUE)
#   A tibble: 7 × 3
#   class      number percent
#   <chr>       <int>   <dbl>
# 1 suv            62  0.266 
# 2 compact        46  0.197 
# 3 midsize        41  0.176 
# 4 subcompact     35  0.150 
# 5 pickup         33  0.142 
# 6 minivan        11  0.0472
# 7 2seater         5  0.0215
```

## 3. Comparison functions

### `cat_compare`

`cat_compare` calculates the distribution of one categorical variable within the groups of another categorical variable and returns the counts and percentages as a tibble. It is essentially a cross tabulation of the two variables with column-wise percentages. Call the function with a dataframe and provide:

1. the `row_cat` -- the variable to distribute down the rows
2. the `col_cat` -- the variable to split into groups along the columns

```r
# Load tabbycat and the mpg dataset
library(tabbycat)
mpg <- ggplot2::mpg

cat_compare(mpg, "class", "cyl")
#  A tibble: 7 × 9
#   class        n_4   n_5   n_6   n_8    p_4   p_5    p_6    p_8
#   <chr>      <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>
# 1 2seater        0     0     0     5 0        0   0      0.0714
# 2 compact       32     2    13     0 0.395    0.5 0.165  0     
# 3 midsize       16     0    23     2 0.198    0   0.291  0.0286
# 4 minivan        1     0    10     0 0.0123   0   0.127  0     
# 5 pickup         3     0    10    20 0.0370   0   0.127  0.286 
# 6 subcompact    21     2     7     5 0.259    0.5 0.0886 0.0714
# 7 suv            8     0    16    38 0.0988   0   0.203  0.543

```

### `cat_contrast`

`cat_contrast` caculates the frequency of discrete values in one categorical variable for each of two mutually exclusive groups within another categorical variable and returns the counts and percentages as a tibble. This lets you see if the distribution of a variable within a particular group differs from the distribution in the rest of the dataset. Call the function with a dataframe and provide:

1. the `row_cat` -- the variable to distribute down the rows 
2. the `col_cat` -- the variable to split into two exclusive groups along the columns
3. the `col_group` -- the name of the group in `col_cat` to contrast against the rest of the dataset

``` r
# Load tabbycat and the mpg dataset
library(tabbycat)
mpg <- ggplot2::mpg

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

### 3.1. NA handling for comparison functions

By default, the frequency of NAs is included in both the row and column results. So there is a row for observations containing NAs in the `row_cat`, and columns showing the number and percentage of NAs found in `col_cat` for each group in `row_cat`.

The default behaviour can be changed through three boolean arguments: `na.rm.row`, `na.rm.col`, and `na.rm`. Setting each of these arguments to FALSE has the following effects:

- `na.rm.row` -- removes the row for NAs from the row results
- `na.rm.col` -- removes the columns for NAs from the column results
- `na.rm` -- removes both the rows and columns of NAs from the results

Note that while removing the columns for NAs from the column results simply changes which columns are shown in the results table, removing the row for NAs from the row results affects the data in the table, because the percentage frequencies are calculated based on the rows shown. In other words, `na.rm.row` lets you calculate the percentage frequencies with or without NAs. This is consistent with the behaviour for `cat_count` and `cat_vcount`. 

The `na.rm` function is a convenience which simply sets `na.rm.row` and `na.rm.col` to the same value. If it is set, it takes priority over both of those areguments, otherwise it is ignored.

### 3.2. Labelling for comparison functions

The comparison functions need names to use as labels for the NA columns, and in the case of `cat_contrast`, for the columns showing frequencies for the observations that aren't in the target group. These labels are controlled with the arguments `na_label` and `other_label`. The default values are "na" and "other" respectively, but you can change them if they colllide with data in your dataset. 

## 4. Summarising functions

### `cat_summarise`

`cat_contrast` calculates summary statistics for a numerical variable for each group within a categorical variable. Call the function with a dataframe and provide:

1. `cat` -- the categorical variable for which summaries will be calculated
2. `num` -- the numerical variable to summarise

``` r
# Load tabbycat and the mpg dataset
library(tabbycat)
mpg <- ggplot2::mpg

cat_summarise(mpg, "class", "hwy")
#   A tibble: 7 × 10
#   class          n    na  mean    sd   min    lq   med    uq   max
#   <chr>      <int> <int> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <int>
# 1 2seater        5     0  24.8  1.30    23  24    25    26      26
# 2 compact       47     0  28.3  3.78    23  26    27    29      44
# 3 midsize       41     0  27.3  2.14    23  26    27    29      32
# 4 minivan       11     0  22.4  2.06    17  22    23    24      24
# 5 pickup        33     0  16.9  2.27    12  16    17    18      22
# 6 subcompact    35     0  28.1  5.38    20  24.5  26    30.5    44
# 7 suv           62     0  18.1  2.98    12  17    17.5  19      27
```

### 4.1. NA handling for `cat_summarise`

In `cat_summarise` NAs are **always** ignored in calculating the summary statistics for each group. But the number of NAs in each group is shown in a column in the table so you can see the potential impact of NAs on the calculation of these statistics.

By default, a row showing summary statistics for observations in `cat` that are NA is included in the table, but this can be turned off by setting `na.rm` to `TRUE`. 

You can see these behaviours in the following example.

``` r
# Set the class of the first three observations to NA
mpg[1:3, ]$class <- NA

# Set the hwy (miles per gallon) of the fourth observation to NA
mpg[4, ]$hwy <- NA

# Call cat_summarise with defaults
cat_summarise(mpg, "class", "hwy")
#   A tibble: 8 × 10
#   class          n    na  mean    sd   min    lq   med    uq   max
#   <chr>      <int> <int> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <int>
# 1 2seater        5     0  24.8  1.30    23  24    25    26      26
# 2 compact       44     1  28.2  3.92    23  26    27    29      44
# 3 midsize       41     0  27.3  2.14    23  26    27    29      32
# 4 minivan       11     0  22.4  2.06    17  22    23    24      24
# 5 pickup        33     0  16.9  2.27    12  16    17    18      22
# 6 subcompact    35     0  28.1  5.38    20  24.5  26    30.5    44
# 7 suv           62     0  18.1  2.98    12  17    17.5  19      27
# 8 NA             3     0  29.7  1.15    29  29    29    30      31

# Call cat_summarise with na.rm set to TRUE
# cat_summarise(mpg, "class", "hwy", na.rm = TRUE)
#   A tibble: 7 × 10
#   class          n    na  mean    sd   min    lq   med    uq   max
#   <chr>      <int> <int> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <int>
# 1 2seater        5     0  24.8  1.30    23  24    25    26      26
# 2 compact       44     1  28.2  3.92    23  26    27    29      44
# 3 midsize       41     0  27.3  2.14    23  26    27    29      32
# 4 minivan       11     0  22.4  2.06    17  22    23    24      24
# 5 pickup        33     0  16.9  2.27    12  16    17    18      22
# 6 subcompact    35     0  28.1  5.38    20  24.5  26    30.5    44
# 7 suv           62     0  18.1  2.98    12  17    17.5  19      27
```

## 5. Other API features

To be written ...

### 5.1. `clean_names`

### 5.2. `only`
