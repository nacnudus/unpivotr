# Display cells as though in a spreadsheet

Takes the 'melted' output of
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
or
[`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html)
(each row represents one cell) and projects the cells into their
original positions. By default this prints to the terminal/console, but
with `display = "browser"` or `display = "rstudio"` it will be displayed
in the browser or the RStudio viewer pane.

This is for viewing only; the output is not designed to be used in other
functions.

Example: The following cells

    row col value
      1   1   "a"
      1   2   "b"
      2   1   "c"
      2   2   "d"

Would be presented as

    row/col 1(A) 2(B)
          1 "a"  "b"
          2 "c"  "d"

The letters in the column names are for comparing this view with a
spreadsheet application.

## Usage

``` r
rectify(cells, values = NULL, types = data_type, formatters = list())

# S3 method for class 'cell_grid'
print(x, display = "terminal", ...)
```

## Arguments

- cells:

  Data frame or tbl, the cells to be displayed.

- values:

  Optional. The column of `cells` to use as the values of each cell.
  Given as a bare variable name. If omitted (the default), the `types`
  argument will be used instead.

- types:

  The column of `cells` that names, for each cell, which column to use
  for the value of the cell. E.g. a cell with a character value will
  have `"character"` in this column.

- formatters:

  A named list of functions to format cell values for display, named
  according to the column that the cell value is in.

- x:

  The output of `rectify()`

- display:

  One of `"terminal"` (default), `"browser"`, `"rstudio"`. To display in
  the browser you must have the `DT` package installed.

- ...:

  Arguments passed on to [`print()`](https://rdrr.io/r/base/print.html)

## Functions

- `print(cell_grid)`: S3 method for class `cell_grid`

## Examples

``` r
x <- data.frame(name = c("Matilda", "Nicholas"),
                score = c(14L, 10L),
                stringsAsFactors = FALSE)

# This is the original form of the table, which is easy to read.
x
#>       name score
#> 1  Matilda    14
#> 2 Nicholas    10

# This is the 'tidy' arrangement that is difficult for humans to read (but
# easy for computers)
y <- as_cells(x, col_names = TRUE)
y
#> # A tibble: 6 × 5
#>     row   col data_type chr        int
#>   <int> <int> <chr>     <chr>    <int>
#> 1     1     1 chr       name        NA
#> 2     2     1 chr       Matilda     NA
#> 3     3     1 chr       Nicholas    NA
#> 4     1     2 chr       score       NA
#> 5     2     2 int       NA          14
#> 6     3     2 int       NA          10

# rectify() projects the cells as a spreadsheet again, for humans to read.
rectify(y)
#> # A tibble: 3 × 3
#>   `row/col` `1(A)`   `2(B)`
#>       <int> <chr>    <chr> 
#> 1         1 name     score 
#> 2         2 Matilda  14    
#> 3         3 Nicholas 10    

# You can choose to use a particular column of the data
rectify(y, values = chr)
#> # A tibble: 3 × 3
#>   `row/col` `1(A)`   `2(B)`
#>       <int> <chr>    <chr> 
#> 1         1 name     score 
#> 2         2 Matilda  NA    
#> 3         3 Nicholas NA    
rectify(y, values = int)
#> # A tibble: 3 × 3
#>   `row/col` `1(A)` `2(B)`
#>       <int>  <int>  <int>
#> 1         1     NA     NA
#> 2         2     NA     14
#> 3         3     NA     10

# You can also show which row or which column each cell came from, which
# helps with understanding what this function does.
rectify(y, values = row)
#> # A tibble: 3 × 3
#>   `row/col` `1(A)` `2(B)`
#>       <int>  <int>  <int>
#> 1         1      1      1
#> 2         2      2      2
#> 3         3      3      3
rectify(y, values = col)
#> # A tibble: 3 × 3
#>   `row/col` `1(A)` `2(B)`
#>       <int>  <int>  <int>
#> 1         1      1      2
#> 2         2      1      2
#> 3         3      1      2

# Empty rows and columns up to the first occupied cell are dropped, but the
# row and column names reflect the original row and column numbers.
y$row <- y$row + 5
y$col <- y$col + 5
rectify(y)
#> # A tibble: 3 × 3
#>   `row/col` `6(F)`   `7(G)`
#>       <dbl> <chr>    <chr> 
#> 1         6 name     score 
#> 2         7 Matilda  14    
#> 3         8 Nicholas 10    

# Supply named functions to format cell values for display.
rectify(y, formatters = list(chr = toupper, int = ~ . * 10))
#> # A tibble: 3 × 3
#>   `row/col` `6(F)`   `7(G)`
#>       <dbl> <chr>    <chr> 
#> 1         6 NAME     SCORE 
#> 2         7 MATILDA  140   
#> 3         8 NICHOLAS 100   
#
# Print in the browser or in the RStudio viewer pane
if (FALSE) { # \dontrun{
  z <- rectify(y)
  print(z, "browser")
  print(z, "rstudio")
} # }
```
