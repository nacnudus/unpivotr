# Divide a grid of cells into partitions containing individual tables

Given the positions of corner cells that mark individual tables in a
single spreadsheet, `partion()` works out which table cells belong to
which corner cells. The individual tables can then be worked on
independently.

`partition()` partitions along both dimensions (rows and columns) at
once. `partition_dim()` partitions along one dimension at a time.

## Usage

``` r
partition(cells, corners, align = "top_left", nest = TRUE, strict = TRUE)

partition_dim(positions, cutpoints, bound = "upper")
```

## Arguments

- cells:

  Data frame or tbl, the cells to be partitioned, from
  [`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
  or
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html).

- corners:

  usually a subset of `cells`, being the corners of individual tables.
  Can also be cells that aren't among `cells`, in which case see the
  `strict` argument.

- align:

  Character, the position of the corner cells relative to their tables,
  one of `"top_left"` (default), `"top_right"`, `"bottom_left"`,
  `"bottom_right"`.

- nest:

  Logical, whether to nest the partitions in a list-column of data
  frames.

- strict:

  Logical, whether to omit partitions that don't contain a corner cell.

- positions:

  Integer vector, the positions of cells (either the row position or the
  column position), which are to be grouped between cutpoints.

- cutpoints:

  Integer vector. The `positions` will be separated into groups either
  side of each cutpoint.

- bound:

  One of `"upper"` or `"lower"`, controls whether cells that lie on a
  cutpoint are should be grouped with cells below or above the cutpoint.
  For example, if column 5 is a cutpoint, and a cell is in column 5,
  `"lower"` would group it with cells in columns 1 to 4, whereas
  `"upper"` would group it with cells in columns 6 to 10. This is so
  that you can use cells at the bottom or the right-hand side of a table
  as the cutpoints (either of which would be 'upper' bounds because row
  and column numbers count from 1 in the top-left row and column). When
  `"upper"`, any `cell_positions` above the first cutpoint will be in
  group 0; when `"lower"`, any `cell_positions` below the final cutpoint
  will be 0.

## Value

`partition_dim()` returns an integer vector, numbering the groups of
cells. Group 0 represents the cells above the first cutpoint (when
`bound = "upper"`), or below the first cutpoint (when
`bound = "lower"`). The other groups are numbered from 1, where group 1
is adjacent to group 0.

`partition_dim()` returns an integer vector, numbering the groups of
cells. Group 0 represents the cells above the first cutpoint (when
`bound = "upper"`), or below the first cutpoint (when
`bound = "lower"`). The other groups are numbered from 1, where group 1
is adjacent to group 0. Divide a grid of cells into chunks along both
dimensions

## Functions

- `partition_dim()`: Divide a grid of cells into chunks along one
  dimension

## Examples

``` r
# The `purpose` dataset, represented in four summary tables
multiples <- purpose$small_multiples
rectify(multiples, character, numeric)
#> # A tibble: 14 × 6
#>    `row/col` `1(A)`                     `2(B)` `3(C)` `4(D)`            `5(E)`
#>        <int> <chr>                      <chr>  <chr>  <chr>             <chr> 
#>  1         1 Postgraduate qualification NA     NA     Bachelor's degree NA    
#>  2         2 Sex                        Value  NA     Sex               Value 
#>  3         3 Female                     NA     NA     Female            NA    
#>  4         4 Male                       NA     NA     Male              NA    
#>  5         5 NA                         NA     NA     NA                NA    
#>  6         6 Diploma                    NA     NA     Certificate       NA    
#>  7         7 Sex                        Value  NA     Sex               Value 
#>  8         8 Female                     NA     NA     Female            NA    
#>  9         9 Male                       NA     NA     Male              NA    
#> 10        10 NA                         NA     NA     NA                NA    
#> 11        11 No Qualification           NA     NA     NA                NA    
#> 12        12 Sex                        Value  NA     NA                NA    
#> 13        13 Female                     NA     NA     NA                NA    
#> 14        14 Male                       NA     NA     NA                NA    

# The same thing in its raw 'melted' form that can be filtered
multiples
#>    row col data_type                  character numeric
#> 1    1   1 character Postgraduate qualification      NA
#> 2    1   2     blank                       <NA>      NA
#> 3    1   4 character          Bachelor's degree      NA
#> 4    1   5     blank                       <NA>      NA
#> 5    2   1 character                        Sex      NA
#> 6    2   2 character                      Value      NA
#> 7    2   4 character                        Sex      NA
#> 8    2   5 character                      Value      NA
#> 9    3   1 character                     Female      NA
#> 10   3   2   numeric                       <NA>  171000
#> 11   3   4 character                     Female      NA
#> 12   3   5   numeric                       <NA>  275000
#> 13   4   1 character                       Male      NA
#> 14   4   2   numeric                       <NA>  159000
#> 15   4   4 character                       Male      NA
#> 16   4   5   numeric                       <NA>  200000
#> 17   6   1 character                    Diploma      NA
#> 18   6   2     blank                       <NA>      NA
#> 19   6   4 character                Certificate      NA
#> 20   6   5     blank                       <NA>      NA
#> 21   7   1 character                        Sex      NA
#> 22   7   2 character                      Value      NA
#> 23   7   4 character                        Sex      NA
#> 24   7   5 character                      Value      NA
#> 25   8   1 character                     Female      NA
#> 26   8   2   numeric                       <NA>  210000
#> 27   8   4 character                     Female      NA
#> 28   8   5   numeric                       <NA>  732000
#> 29   9   1 character                       Male      NA
#> 30   9   2   numeric                       <NA>  173000
#> 31   9   4 character                       Male      NA
#> 32   9   5   numeric                       <NA>  807000
#> 33  11   1 character           No Qualification      NA
#> 34  11   2     blank                       <NA>      NA
#> 35  12   1 character                        Sex      NA
#> 36  12   2 character                      Value      NA
#> 37  13   1 character                     Female      NA
#> 38  13   2   numeric                       <NA>  344000
#> 39  14   1 character                       Male      NA
#> 40  14   2   numeric                       <NA>  287000

# First, find the cells that mark a corner of each table
corners <-
  dplyr::filter(multiples,
                !is.na(character),
                !(character %in% c("Sex", "Value", "Female", "Male")))

# Then find out which cells fall into which partition
partition(multiples, corners)
#> # A tibble: 5 × 6
#>   corner_row corner_col cells            data_type character             numeric
#>        <dbl>      <dbl> <list>           <chr>     <chr>                   <dbl>
#> 1          1          1 <tibble [8 × 5]> character Postgraduate qualifi…      NA
#> 2          1          4 <tibble [8 × 5]> character Bachelor's degree          NA
#> 3          6          1 <tibble [8 × 5]> character Diploma                    NA
#> 4          6          4 <tibble [8 × 5]> character Certificate                NA
#> 5         11          1 <tibble [8 × 5]> character No Qualification           NA

# You can also use bottom-left corners (or top-right or bottom-right)
bl_corners <- dplyr::filter(multiples, character == "Male")
partition(multiples, bl_corners, align = "bottom_left")
#> # A tibble: 5 × 6
#>   corner_row corner_col cells            data_type character numeric
#>        <dbl>      <dbl> <list>           <chr>     <chr>       <dbl>
#> 1          4          1 <tibble [8 × 5]> character Male           NA
#> 2          4          4 <tibble [8 × 5]> character Male           NA
#> 3          9          1 <tibble [8 × 5]> character Male           NA
#> 4          9          4 <tibble [8 × 5]> character Male           NA
#> 5         14          1 <tibble [8 × 5]> character Male           NA

# To complete the grid even when not all corners are supplied, use `strict`
bl_corners <- bl_corners[-1, ]
partition(multiples, bl_corners, align = "bottom_left")
#> # A tibble: 4 × 6
#>   corner_row corner_col cells            data_type character numeric
#>        <dbl>      <dbl> <list>           <chr>     <chr>       <dbl>
#> 1          4          4 <tibble [8 × 5]> character Male           NA
#> 2          9          1 <tibble [8 × 5]> character Male           NA
#> 3          9          4 <tibble [8 × 5]> character Male           NA
#> 4         14          1 <tibble [8 × 5]> character Male           NA
partition(multiples, bl_corners, align = "bottom_left", strict = FALSE)
#> # A tibble: 4 × 6
#>   corner_row corner_col cells            data_type character numeric
#>        <dbl>      <dbl> <list>           <chr>     <chr>       <dbl>
#> 1          4          4 <tibble [8 × 5]> character Male           NA
#> 2          9          1 <tibble [8 × 5]> character Male           NA
#> 3          9          4 <tibble [8 × 5]> character Male           NA
#> 4         14          1 <tibble [8 × 5]> character Male           NA
# Given a set of cells in rows 1 to 10, partition them at the 3rd, 5th and 7th
# rows.
partition_dim(1:10, c(3, 5, 7))
#>  [1] NA NA  3  3  5  5  7  7  7  7

# Given a set of cells in columns 1 to 10, partition them at the 3rd, 5th and
# 7th column.  This example is exactly the same as the previous one, to show
# that the function works the same way on columns as rows.
partition_dim(1:10, c(3, 5, 7))
#>  [1] NA NA  3  3  5  5  7  7  7  7

# Given a set of cells in rows 1 to 10, partition them at the 3rd, 5th and
# 7th rows, aligned to the bottom of the group.
partition_dim(1:10, c(3, 5, 7), bound = "lower")
#>  [1]  3  3  3  5  5  7  7 NA NA NA

# Non-integer row/column numbers and cutpoints can be used, even though they
# make no sense in the context of partioning grids of cells.  They are
# rounded towards zero first.
partition_dim(1:10 - .5, c(3, 5, 7))
#>  [1] NA NA NA  3  3  5  5  7  7  7
partition_dim(1:10, c(3, 5, 7) + 1.5)
#>  [1] NA NA NA NA  4  4  6  6  8  8
```
