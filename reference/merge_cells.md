# Merge cell values into a single cell by rows or columns

When a single column header is split across cells, merge the cells with
`merge_rows()` or `merge_cols()`. E.g. if a column header "Mean GDP" is
split over two cells, where the top cell has the value "Mean" and the
bottom cell has the value "GDP", then `merge_rows()` will combine them
into a single cell with the value "Mean GDP".

`merge_rows()` keeps the top cell, and `merge_cols()` keeps the
left-most cell. When there are several columns of headers,
`merge_rows()` aligns the output cells so that they are all in the same
row, and similarly `merge_cols()` aligns to the same column.

These functions apply only to cells with character values because it
doesn't make sense to concatenate non-character values. Convert cell
values to characters first if you need to merge non-character cells.

Columns other than the one that is concatenated will take the value of
the top or left-most cell. For example, if `cells` has the columns
`data_type` and `is_blank`, and the top or left-most cell has the values
`blank` or `TRUE` in those columns, then the resulting merged cell will
also have the values `blank` or `TRUE` in those columns, even if the
concatenated value is no longer blank.

## Usage

``` r
merge_rows(cells, rows, values, collapse = " ")

merge_cols(cells, cols, values, collapse = " ")
```

## Arguments

- cells:

  Data frame. The cells of a pivot table, usually the output of
  [`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
  or
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html),
  or of a subsequent operation on those outputs.

- rows:

  The numbers of the rows to be merged.

- values:

  The column of `cells` to use as the values of each cell to be merged.
  Given as a bare variable name.

- collapse:

  A character string to separate the values of each cell.

- cols:

  The numbers of the columns to be merged.

## Value

A data frame

## Examples

``` r
 x <- tibble::tribble(
~row, ~col, ~data_type,     ~chr,
   1,    1,      "chr",   "Katy",
   2,    1,      "chr",  "Perry",
   3,    1,      "chr",      "a",
   4,    1,      "chr",      "b",
   5,    1,      "chr",      "c",
   2,    2,      "chr",  "Adele",
   3,    2,      "chr",      "d",
   4,    2,      "chr",      "e",
   5,    2,      "chr",      "f",
   1,    3,      "chr", "Ariana",
   2,    3,      "chr", "Grande",
   3,    3,      "chr",      "g",
   4,    3,      "chr",      "h",
   5,    3,      "chr",      "i"
)
rectify(x)
#> # A tibble: 5 × 4
#>   `row/col` `1(A)` `2(B)` `3(C)`
#>       <dbl> <chr>  <chr>  <chr> 
#> 1         1 Katy   NA     Ariana
#> 2         2 Perry  Adele  Grande
#> 3         3 a      d      g     
#> 4         4 b      e      h     
#> 5         5 c      f      i     
y <- merge_rows(x, 1:2, chr)
rectify(y)
#> # A tibble: 5 × 4
#>   `row/col` `1(A)`     `2(B)` `3(C)`       
#>       <dbl> <chr>      <chr>  <chr>        
#> 1         1 Katy Perry Adele  Ariana Grande
#> 2         2 NA         NA     NA           
#> 3         3 a          d      g            
#> 4         4 b          e      h            
#> 5         5 c          f      i            
z <- merge_cols(x, 1:2, chr)
rectify(z)
#> # A tibble: 5 × 4
#>   `row/col` `1(A)`      `2(B)` `3(C)`
#>       <dbl> <chr>       <lgl>  <chr> 
#> 1         1 Katy        NA     Ariana
#> 2         2 Perry Adele NA     Grande
#> 3         3 a d         NA     g     
#> 4         4 b e         NA     h     
#> 5         5 c f         NA     i     
```
