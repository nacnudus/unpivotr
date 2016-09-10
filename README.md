---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# unpivotr

Tools for converting data from complex or irregular layouts to a columnar
structure.  For example, tables with multi-level column or row headers, or
spreadsheets.  Header and data cells are selected by their contents, position
and formatting, and are associated with one other by their relative positions.

## Installation


```r
devtools::install_github("nacnudus/tidyxl")
```

## Example


```r
library(unpivotr)
library(tidyxl)
library(readxl)
library(dplyr)
```

The package includes a spreadsheet, 'purpose.xlsx', which has tables that have
multi-row column headers and multi-column row-headers.  A popular package for
importing spreadsheets does the following:


```r
read_excel("./inst/extdata/purpose.xlsx", "NNW WNW")
#> # A tibble: 22 x 6
#>                              Female     NA  Male     NA
#>                <chr>   <chr>  <chr>  <chr> <chr>  <chr>
#> 1               <NA>    <NA>  0 - 6 7 - 10 0 - 6 7 - 10
#> 2  Bachelor's degree 15 - 24   7000  27000  <NA>  13000
#> 3               <NA> 25 - 44  12000 137000  9000  81000
#> 4               <NA> 45 - 64  10000  64000  7000  66000
#> 5               <NA>     65+   <NA>  18000  7000  17000
#> 6        Certificate 15 - 24  29000 161000 30000 190000
#> 7               <NA> 25 - 44  34000 179000 31000 219000
#> 8               <NA> 45 - 64  30000 210000 23000 199000
#> 9               <NA>     65+  12000  77000  8000 107000
#> 10           Diploma 15 - 24   <NA>  14000  9000  11000
#> # ... with 12 more rows
```

The [tidyxl](https://github.com/nacnudus/tidyxl) package imports the same table
into a format suitable for unpivotr:


```r
(pivoted <- contents("./inst/extdata/purpose.xlsx", "NNW WNW")[[1]])
#> # A tibble: 128 x 14
#>    address   row   col content formula formula_type formula_ref
#>      <chr> <int> <int>   <chr>   <chr>        <chr>       <chr>
#> 1       D2     2     4      16    <NA>         <NA>        <NA>
#> 2       E2     2     5    <NA>    <NA>         <NA>        <NA>
#> 3       F2     2     6       6    <NA>         <NA>        <NA>
#> 4       G2     2     7    <NA>    <NA>         <NA>        <NA>
#> 5       D3     3     4       9    <NA>         <NA>        <NA>
#> 6       E3     3     5      20    <NA>         <NA>        <NA>
#> 7       F3     3     6       9    <NA>         <NA>        <NA>
#> 8       G3     3     7      20    <NA>         <NA>        <NA>
#> 9       B4     4     2      13    <NA>         <NA>        <NA>
#> 10      C4     4     3       7    <NA>         <NA>        <NA>
#> # ... with 118 more rows, and 7 more variables: formula_group <int>,
#> #   type <chr>, character <chr>, height <dbl>, width <dbl>,
#> #   style_format_id <dbl>, local_format_id <dbl>
```

We can identify each row of column headers, each column of row headers, and the
data cells, using any method.  Here, we use some
[dplyr](https://github.com/hadley/dplyr) and some base R functions.


```r
col_headers <- 
  pivoted %>%
  filter(row <= 3, !is.na(content)) %>%
  select(row, col, header = character) %>%
  split(.$row)
col_headers 
#> $`2`
#> # A tibble: 2 x 3
#>     row   col header
#>   <int> <int>  <chr>
#> 1     2     4 Female
#> 2     2     6   Male
#> 
#> $`3`
#> # A tibble: 4 x 3
#>     row   col header
#>   <int> <int>  <chr>
#> 1     3     4  0 - 6
#> 2     3     5 7 - 10
#> 3     3     6  0 - 6
#> 4     3     7 7 - 10

row_headers <- 
  pivoted %>%
  filter(col <= 3, !is.na(content)) %>%
  select(row, col, header = character) %>%
  split(.$col)
row_headers
#> $`2`
#> # A tibble: 5 x 3
#>     row   col                     header
#>   <int> <int>                      <chr>
#> 1     4     2          Bachelor's degree
#> 2     8     2                Certificate
#> 3    12     2                    Diploma
#> 4    16     2           No Qualification
#> 5    20     2 Postgraduate qualification
#> 
#> $`3`
#> # A tibble: 20 x 3
#>      row   col  header
#>    <int> <int>   <chr>
#> 1      4     3 15 - 24
#> 2      5     3 25 - 44
#> 3      6     3 45 - 64
#> 4      7     3     65+
#> 5      8     3 15 - 24
#> 6      9     3 25 - 44
#> 7     10     3 45 - 64
#> 8     11     3     65+
#> 9     12     3 15 - 24
#> 10    13     3 25 - 44
#> 11    14     3 45 - 64
#> 12    15     3     65+
#> 13    16     3 15 - 24
#> 14    17     3 25 - 44
#> 15    18     3 45 - 64
#> 16    19     3     65+
#> 17    20     3 15 - 24
#> 18    21     3 25 - 44
#> 19    22     3 45 - 64
#> 20    23     3     65+

datacells <- 
  pivoted %>%
  filter(row >= 4, col >= 4) %>%
  select(row, col, value = as.integer(content))
datacells
#> # A tibble: 80 x 3
#>      row   col  value
#>    <int> <int>  <chr>
#> 1      4     4   7000
#> 2      4     5  27000
#> 3      4     6   <NA>
#> 4      4     7  13000
#> 5      5     4  12000
#> 6      5     5 137000
#> 7      5     6   9000
#> 8      5     7  81000
#> 9      6     4  10000
#> 10     6     5  64000
#> # ... with 70 more rows
```

Using `unpivotr` functions, we associate the data cells with the headers, by
proximity in given compass directions.


```r
unpivoted <- 
  datacells %>%
  NNW(col_headers[[1]], "sex") %>%
  N(col_headers[[2]], "purpose") %>%
  WNW(row_headers[[1]], "education") %>% 
  W(row_headers[[2]], "age")
unpivoted
#> # A tibble: 80 x 7
#>      row   col  value    sex purpose         education     age
#>    <dbl> <dbl>  <chr>  <chr>   <chr>             <chr>   <chr>
#> 1      4     4   7000 Female   0 - 6 Bachelor's degree 15 - 24
#> 2      4     5  27000 Female  7 - 10 Bachelor's degree 15 - 24
#> 3      4     6   <NA>   Male   0 - 6 Bachelor's degree 15 - 24
#> 4      4     7  13000   Male  7 - 10 Bachelor's degree 15 - 24
#> 5      5     4  12000 Female   0 - 6 Bachelor's degree 25 - 44
#> 6      5     5 137000 Female  7 - 10 Bachelor's degree 25 - 44
#> 7      5     6   9000   Male   0 - 6 Bachelor's degree 25 - 44
#> 8      5     7  81000   Male  7 - 10 Bachelor's degree 25 - 44
#> 9      6     4  10000 Female   0 - 6 Bachelor's degree 45 - 64
#> 10     6     5  64000 Female  7 - 10 Bachelor's degree 45 - 64
#> # ... with 70 more rows
```

We can re-pivot the final data in R using `ftable()` to check that it has been
imported correctly.


```r
ftable(unpivoted, 
       row.vars = c("education", "age"),
       col.vars = c("sex", "purpose"))
#>                                    sex     Female         Male       
#>                                    purpose  0 - 6 7 - 10 0 - 6 7 - 10
#> education                  age                                       
#> Bachelor's degree          15 - 24              1      1     0      1
#>                            25 - 44              1      1     1      1
#>                            45 - 64              1      1     1      1
#>                            65+                  0      1     1      1
#> Certificate                15 - 24              1      1     1      1
#>                            25 - 44              1      1     1      1
#>                            45 - 64              1      1     1      1
#>                            65+                  1      1     1      1
#> Diploma                    15 - 24              0      1     1      1
#>                            25 - 44              1      1     1      1
#>                            45 - 64              1      1     1      1
#>                            65+                  1      1     1      1
#> No Qualification           15 - 24              1      1     1      1
#>                            25 - 44              1      1     1      1
#>                            45 - 64              1      1     1      1
#>                            65+                  1      1     1      1
#> Postgraduate qualification 15 - 24              0      1     0      0
#>                            25 - 44              1      1     1      1
#>                            45 - 64              1      1     1      1
#>                            65+                  0      1     0      1
read_excel("./inst/extdata/purpose.xlsx", "NNW WNW")
#> # A tibble: 22 x 6
#>                              Female     NA  Male     NA
#>                <chr>   <chr>  <chr>  <chr> <chr>  <chr>
#> 1               <NA>    <NA>  0 - 6 7 - 10 0 - 6 7 - 10
#> 2  Bachelor's degree 15 - 24   7000  27000  <NA>  13000
#> 3               <NA> 25 - 44  12000 137000  9000  81000
#> 4               <NA> 45 - 64  10000  64000  7000  66000
#> 5               <NA>     65+   <NA>  18000  7000  17000
#> 6        Certificate 15 - 24  29000 161000 30000 190000
#> 7               <NA> 25 - 44  34000 179000 31000 219000
#> 8               <NA> 45 - 64  30000 210000 23000 199000
#> 9               <NA>     65+  12000  77000  8000 107000
#> 10           Diploma 15 - 24   <NA>  14000  9000  11000
#> # ... with 12 more rows
```

The functions `extend()` and `offset` can be used when selecting cells relative
to an intial anchor cell (or group of cells, called a 'bag').

## Similar projects

[unpivotr](https://github.com/nacnudus/unpivotr) is inspired by the United
Kingdom Office of National Statistics
[ONSdatabaker](https://github.com/ONS-OpenData/ONSdatabaker), which is
implemented in Python.  [unpivotr](https://github.com/nacnudus/unpivotr) differs
already by using compass directions, and further developments are planned.

The [rsheets](https://github.com/rsheets) project of several R packages is in
the early stages of importing spreadsheet information from Excel and Google
Sheets into R, manipulating it, and potentially parsing and processing formulas
and writing out to spreadsheet files.  In particular,
[jailbreaker](https://github.com/rsheets/jailbreakr) attempts to extract
non-tabular data from spreadsheets into tabular structures automatically via
some clever algorithms.

[unpivot](https://github.com/nacnudus/unpivotr) differs from
[jailbreaker](https://github.com/rsheets/jailbreakr) in that it provides tools
for unpivoting complex and non-tabular data layouts using I not AI
(intelligence, not artificial intelligence).

## Roadmap

- [ ] Implement boundaries for the `ABOVE()`, `BELOW()`, `LEFT()`, `RIGHT()`
functions to search within boundaries for a header cell, e.g. within an area
bounded by borders.  This will make it possible in many cases to break ties
between header cells that are equidistant from a data cell.
