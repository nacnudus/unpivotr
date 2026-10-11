# Pack cell values from separate columns per data type into one list-column

Pack cell values from separate columns per data type into one
list-column

## Usage

``` r
pack(
  cells,
  types = data_type,
  name = "value",
  drop_types = TRUE,
  drop_type_cols = TRUE
)

unpack(cells, values = value, name = "data_type", drop_packed = TRUE)
```

## Arguments

- cells:

  A data frame of cells, one row per cell. For `pack()` it must have a
  column that names, for each cell/row, which of the other columns the
  value is in. For `unpack()` it must have a list-column of cell values,
  where each element is named according to the data type of the value.

- types:

  For `pack()`, the name of the column that that names, for each
  cell/row, which of the other columns the value is in.

- name:

  A string. For `pack()`, the name to give the new list-column of
  values. For `unpack()`, the name to give the new column that will
  name, for each cell, which of the other columns the value is in.

- drop_types:

  For `pack()`, whether to drop the column named by `types`.

- drop_type_cols:

  For `pack()`, whether to drop the original columns of cell values.

- values:

  For `unpack()`, the name of the list-column of cell values.

- drop_packed:

  For `unpack()`, whether to drop the column named by `values`.

## Details

When cells are represented by rows of a data frame, the values of the
cells will be in different columns according to their data type. For
example, the value of a cell containing text will be in a column called
`chr` (or `character` if it came via tidyxl). A column called
`data_type` names, for each cell, which column its value is in.

`pack()` rearranges the cell values in a different way, so that they are
all in one column, by

1.  taking each cell value, from whichever column.

2.  making it an element of a list.

3.  naming each element according to the column it came from.

4.  making the list into a new list-column of the original data frame.

By default, the original columns are dropped, and so is the `data_type`
column.

`unpack()` is the complement.

This can be useful for dropping all columns of `cells` except the ones
that contain data. For example,
[`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html)
returns a very wide data frame, and to make it narrow you might do:

    select(cells, row, col, character, numeric, date)

But what if you don't know in advance that the data types you need are
`character`, `numeric` and `date`? You might also need `logical` and
`error`.

Instead, `pack()` all the data types into a single column, select it,
and then unpack.

    pack(cells) %>%
      select(row, col, value) %>%
      unpack()

## Functions

- `unpack()`: Unpack cell values from one list-column into separate
  columns per data type

## Examples

``` r
# A normal data frame
w <- data.frame(foo = 1:2,
                bar = c("a", "b"),
                stringsAsFactors = FALSE)
w
#>   foo bar
#> 1   1   a
#> 2   2   b

# The same data, represented by one row per cell, with integer values in the
# `int` column and character values in the `chr` column.
x <- as_cells(w)
x
#> # A tibble: 4 × 5
#>     row   col data_type chr     int
#>   <int> <int> <chr>     <chr> <int>
#> 1     1     1 int       NA        1
#> 2     2     1 int       NA        2
#> 3     1     2 chr       a        NA
#> 4     2     2 chr       b        NA

# pack() and unpack() are complements
pack(x)
#> # A tibble: 4 × 3
#>     row   col value       
#>   <int> <int> <named list>
#> 1     1     1 <int [1]>   
#> 2     2     1 <int [1]>   
#> 3     1     2 <chr [1]>   
#> 4     2     2 <chr [1]>   
unpack(pack(x))
#> # A tibble: 4 × 5
#>     row   col data_type chr     int
#>   <int> <int> <chr>     <chr> <int>
#> 1     1     1 int       NA        1
#> 2     2     1 int       NA        2
#> 3     1     2 chr       a        NA
#> 4     2     2 chr       b        NA

# Drop non-data columns from a wide data frame of cells from tidyxl
if (require(tidyxl)) {
  cells <- tidyxl::xlsx_cells(system.file("extdata", "purpose.xlsx", package = "unpivotr"))
  cells

  pack(cells) %>%
    dplyr::select(row, col, value) %>%
    unpack()
}
#> Loading required package: tidyxl
#> # A tibble: 1,721 × 7
#>      row   col data_type blank character                           error numeric
#>    <int> <int> <chr>     <lgl> <chr>                               <chr>   <dbl>
#>  1     1     1 character NA    The data is 'Sense of purpose by h… NA         NA
#>  2     2     1 character NA    It can be found in the section 'Pe… NA         NA
#>  3     3     1 character NA    The description provided by Statis… NA         NA
#>  4     5     1 character NA    The 2014 New Zealand General Socia… NA         NA
#>  5     6     1 character NA    It provides new and redeveloped da… NA         NA
#>  6     7     1 character NA    Symbols used in this table:         NA         NA
#>  7     8     1 character NA    S - Data has been suppressed.       NA         NA
#>  8     9     1 character NA    * - Relative sampling error of 50 … NA         NA
#>  9    10     1 character NA    For more tables using the NZGSS 20… NA         NA
#> 10    11     1 character NA    Data quality                        NA         NA
#> # ℹ 1,711 more rows
```
