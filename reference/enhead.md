# Join data cells to headers

Data cells in a table are associated with header cells by proximity.
`enhead()` joins a data frame of data cells to a data frame of header
cells, choosing the nearest header cells in the given direction. See
[`?direction`](https://nacnudus.github.io/unpivotr/reference/direction.md).

## Usage

``` r
enhead(data_cells, header_cells, direction, drop = TRUE)
```

## Arguments

- data_cells:

  Data frame of data cells with at least the columns 'row' and 'column',
  which are `numeric` or `integer`.

- header_cells:

  Data frame of header cells with at least the columns 'row' and
  'column', which are numeric/integer vectors.

- direction:

  The direction between a data cell and its header, one of `"up"`,
  `"right"`, `"down"`, `"left"`, `"up-left"`, `"up-right"`,
  `"right-up"`, `"right-down"`, `"down-right"`, `"down-left"`,
  `"left-down"`, `"left-up"`, `"up-ish"`, `"down-ish"`, `"left-ish"` and
  `"right-ish"`. See
  [`?direction`](https://nacnudus.github.io/unpivotr/reference/direction.md).

- drop:

  Logical vector length 1. Whether data cells that can't be associated
  with a header should be dropped. Default: `TRUE`.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
# Load some pivoted data
(x <- purpose$`up-left left-up`)
#>                            X2      X3     X4     X5    X6     X7
#> 1                        <NA>    <NA> Female   <NA>  Male   <NA>
#> 2                        <NA>    <NA>  0 - 6 7 - 10 0 - 6 7 - 10
#> 3           Bachelor's degree 15 - 24   7000  27000  <NA>  13000
#> 4                        <NA> 25 - 44  12000 137000  9000  81000
#> 5                        <NA> 45 - 64  10000  64000  7000  66000
#> 6                        <NA>     65+   <NA>  18000  7000  17000
#> 7                 Certificate 15 - 24  29000 161000 30000 190000
#> 8                        <NA> 25 - 44  34000 179000 31000 219000
#> 9                        <NA> 45 - 64  30000 210000 23000 199000
#> 10                       <NA>     65+  12000  77000  8000 107000
#> 11                    Diploma 15 - 24   <NA>  14000  9000  11000
#> 12                       <NA> 25 - 44  10000  66000  8000  47000
#> 13                       <NA> 45 - 64   6000  68000  5000  58000
#> 14                       <NA>     65+   5000  41000  1000  34000
#> 15           No Qualification 15 - 24  10000  43000 12000  37000
#> 16                       <NA> 25 - 44  11000  36000 21000  50000
#> 17                       <NA> 45 - 64  19000  91000 17000  75000
#> 18                       <NA>     65+  16000 118000  9000  66000
#> 19 Postgraduate qualification 15 - 24   <NA>   6000  <NA>   <NA>
#> 20                       <NA> 25 - 44   5000  86000  7000  60000
#> 21                       <NA> 45 - 64   6000  55000  6000  68000
#> 22                       <NA>     65+   <NA>  13000  <NA>  18000
# Make a tidy representation
cells <- as_cells(x)
cells <- cells[!is.na(cells$chr), ]
head(cells)
#> # A tibble: 6 × 4
#>     row   col data_type chr                       
#>   <int> <int> <chr>     <chr>                     
#> 1     3     1 chr       Bachelor's degree         
#> 2     7     1 chr       Certificate               
#> 3    11     1 chr       Diploma                   
#> 4    15     1 chr       No Qualification          
#> 5    19     1 chr       Postgraduate qualification
#> 6     3     2 chr       15 - 24                   
# Select the cells containing the values
data_cells <-
  filter(cells, row >= 3, col >= 3) %>%
  transmute(row, col, count = as.integer(chr))
head(data_cells)
#> # A tibble: 6 × 3
#>     row   col count
#>   <int> <int> <int>
#> 1     3     3  7000
#> 2     4     3 12000
#> 3     5     3 10000
#> 4     7     3 29000
#> 5     8     3 34000
#> 6     9     3 30000
# Select the headers
qualification <-
  filter(cells, col == 1) %>%
  select(row, col, qualification = chr)
age <-
  filter(cells, col == 2) %>%
  select(row, col, age = chr)
gender <-
  filter(cells, row == 1) %>%
  select(row, col, gender = chr)
satisfaction <-
  filter(cells, row == 2) %>%
  select(row, col, satisfaction = chr)
# From each data cell, search for the nearest one of each of the headers
data_cells %>%
  enhead(gender, "up-left") %>%
  enhead(satisfaction, "up") %>%
  enhead(qualification, "left-up") %>%
  enhead(age, "left") %>%
  select(-row, -col)
#> # A tibble: 72 × 5
#>     count gender satisfaction qualification     age    
#>     <int> <chr>  <chr>        <chr>             <chr>  
#>  1   7000 Female 0 - 6        Bachelor's degree 15 - 24
#>  2  12000 Female 0 - 6        Bachelor's degree 25 - 44
#>  3  10000 Female 0 - 6        Bachelor's degree 45 - 64
#>  4  27000 Female 7 - 10       Bachelor's degree 15 - 24
#>  5 137000 Female 7 - 10       Bachelor's degree 25 - 44
#>  6  64000 Female 7 - 10       Bachelor's degree 45 - 64
#>  7  18000 Female 7 - 10       Bachelor's degree 65+    
#>  8   9000 Male   0 - 6        Bachelor's degree 25 - 44
#>  9   7000 Male   0 - 6        Bachelor's degree 45 - 64
#> 10   7000 Male   0 - 6        Bachelor's degree 65+    
#> # ℹ 62 more rows

# The `drop` argument controls what happens when for some cells there is no
# header in the given direction. When `drop = TRUE` (the default), cells that
# can't be joined to a header are dropped.  Otherwise they are kept.
enhead(data_cells, gender, "up")
#> # A tibble: 33 × 4
#>      row   col count gender
#>    <int> <int> <int> <chr> 
#>  1     3     3  7000 Female
#>  2     4     3 12000 Female
#>  3     5     3 10000 Female
#>  4     7     3 29000 Female
#>  5     8     3 34000 Female
#>  6     9     3 30000 Female
#>  7    10     3 12000 Female
#>  8    12     3 10000 Female
#>  9    13     3  6000 Female
#> 10    14     3  5000 Female
#> # ℹ 23 more rows
enhead(data_cells, gender, "up", drop = FALSE)
#> # A tibble: 72 × 4
#>      row   col count gender
#>    <int> <int> <int> <chr> 
#>  1     3     3  7000 Female
#>  2     4     3 12000 Female
#>  3     5     3 10000 Female
#>  4     7     3 29000 Female
#>  5     8     3 34000 Female
#>  6     9     3 30000 Female
#>  7    10     3 12000 Female
#>  8    12     3 10000 Female
#>  9    13     3  6000 Female
#> 10    14     3  5000 Female
#> # ℹ 62 more rows
```
