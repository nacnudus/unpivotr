# Spread key-value pairs of mixed types across multiple columns

`spatter()` is like
[`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html)
but for when different columns have different data types. It works on
data that has come via
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
or
[`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html),
where each row represents one cell of a table, and the value of the cell
is represented in a different column, depending on the data type.

## Usage

``` r
spatter(cells, key, values = NULL, types = data_type, formatters = list())
```

## Arguments

- cells:

  A data frame where each row represents a cell, with columns `row` and
  `col`, usually a column `data_type`, and additional columns of cell
  values.

- key:

  The name of the column whose values will become column names

- values:

  Optional. The column of `cells` to use as the value of each cell.
  Given as a bare variable name. If omitted (the default), the `type`
  argument will be used instead.

- types:

  Optional. The column that names, for each row of `cells`, which column
  contains the cell value. Defaults to `data_type`.

- formatters:

  A named list of functions for formatting particular data types, named
  by the data type (the name of the column of `cells` that contains the
  cell value.

## Examples

``` r
# A tidy representation of cells of mixed data types
x <- data.frame(stringsAsFactors = FALSE,
        row = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
        col = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
  data_type = c("character", "character", "character", "numeric", "character",
                "numeric", "character", "numeric"),
  character = c("Name", "Age", "Matilda", NA, "Nicholas", NA, "Olivia", NA),
    numeric = c(NA, NA, NA, 1, NA, 3, NA, 5))
x
#>   row col data_type character numeric
#> 1   1   1 character      Name      NA
#> 2   1   2 character       Age      NA
#> 3   2   1 character   Matilda      NA
#> 4   2   2   numeric      <NA>       1
#> 5   3   1 character  Nicholas      NA
#> 6   3   2   numeric      <NA>       3
#> 7   4   1 character    Olivia      NA
#> 8   4   2   numeric      <NA>       5

# How it would look in a spreadsheet
rectify(x)
#> # A tibble: 4 × 3
#>   `row/col` `1(A)`   `2(B)`
#>       <int> <chr>    <chr> 
#> 1         1 Name     Age   
#> 2         2 Matilda  1     
#> 3         3 Nicholas 3     
#> 4         4 Olivia   5     

# How it looks after treating the cells in row 1 as headers
y <- behead(x, "N", header)
y$col <- NULL # Drop the 'col' column
y
#> # A tibble: 6 × 5
#>     row data_type character numeric header
#>   <int> <chr>     <chr>       <dbl> <chr> 
#> 1     2 character Matilda        NA Name  
#> 2     2 numeric   NA              1 Age   
#> 3     3 character Nicholas       NA Name  
#> 4     3 numeric   NA              3 Age   
#> 5     4 character Olivia         NA Name  
#> 6     4 numeric   NA              5 Age   

# At this point you might want to do tidyr::spread(), but it won't work because
# you want to use both the `character` and `numeric` columns as the values.
tidyr::spread(y, header, numeric)
#> # A tibble: 6 × 5
#>     row data_type character   Age  Name
#>   <int> <chr>     <chr>     <dbl> <dbl>
#> 1     2 character Matilda      NA    NA
#> 2     2 numeric   NA            1    NA
#> 3     3 character Nicholas     NA    NA
#> 4     3 numeric   NA            3    NA
#> 5     4 character Olivia       NA    NA
#> 6     4 numeric   NA            5    NA
tidyr::spread(y, header, character)
#> # A tibble: 6 × 5
#>     row data_type numeric Age   Name    
#>   <int> <chr>       <dbl> <chr> <chr>   
#> 1     2 character      NA NA    Matilda 
#> 2     2 numeric         1 NA    NA      
#> 3     3 character      NA NA    Nicholas
#> 4     3 numeric         3 NA    NA      
#> 5     4 character      NA NA    Olivia  
#> 6     4 numeric         5 NA    NA      
spatter(y, header)
#> # A tibble: 3 × 3
#>     row   Age Name    
#>   <int> <dbl> <chr>   
#> 1     2     1 Matilda 
#> 2     3     3 Nicholas
#> 3     4     5 Olivia  

# The difference between spatter() and tidyr::spread() is that spatter()
# needs to know which data-type to use for each cell beneath the headers.  By
# default, it looks at the `data_type` column to decide, but you can change
# that with the `types` argument.
y %>%
  dplyr::select(-data_type, -numeric) %>%
  dplyr::mutate(data_type_2 = "character") %>%
  spatter(header, types = data_type_2)
#> # A tibble: 3 × 3
#>     row Age   Name    
#>   <int> <chr> <chr>   
#> 1     2 NA    Matilda 
#> 2     3 NA    Nicholas
#> 3     4 NA    Olivia  

# Alternatively you can name one specific column to use for the cell values.
y %>%
  dplyr::mutate(foo = letters[1:6]) %>%
  dplyr::select(header, row, foo) %>%
  spatter(header, values = foo)
#> # A tibble: 3 × 3
#>     row Age   Name 
#>   <int> <chr> <chr>
#> 1     2 b     a    
#> 2     3 d     c    
#> 3     4 f     e    

# The column used for the values is consumed before the spread occurs. If
# it's necessary for demarking the rows, then make a copy of it first,
# otherwise you'll get an error like "Duplicate identifiers for rows ..."
y %>%
  dplyr::mutate(row2 = row) %>%
  dplyr::select(row, header, row2) %>%
  spatter(header, values = row2)
#> # A tibble: 3 × 3
#>     row   Age  Name
#>   <int> <int> <int>
#> 1     2     2     2
#> 2     3     3     3
#> 3     4     4     4

# Like tidyr::spread(), you need to discard extraneous columns beforehand.
# Otherwise you can get more rows out than you want.
y$extra <- 11:16
spatter(y, header)
#> # A tibble: 6 × 4
#>     row extra   Age Name    
#>   <int> <int> <dbl> <chr>   
#> 1     2    11    NA Matilda 
#> 2     2    12     1 NA      
#> 3     3    13    NA Nicholas
#> 4     3    14     3 NA      
#> 5     4    15    NA Olivia  
#> 6     4    16     5 NA      

# pack() is an easy way to keep just the columns you need, without knowing
# in advance which data-type columns you need.  This examples adds a new
# column, which is then removed by the pack-unpack sequence without having to
# mention it by name.
x$extra <- 11:18
x %>%
  pack() %>%
  dplyr::select(row, col, value) %>%
  unpack()
#>   row col data_type character numeric
#> 1   1   1 character      Name      NA
#> 2   1   2 character       Age      NA
#> 3   2   1 character   Matilda      NA
#> 4   2   2   numeric      <NA>       1
#> 5   3   1 character  Nicholas      NA
#> 6   3   2   numeric      <NA>       3
#> 7   4   1 character    Olivia      NA
#> 8   4   2   numeric      <NA>       5

# spatter() automatically converts data types so that they can coexist in the
# same column.  Ordered factors in particular will always be coerced to
# unordered factors.

# You can control data type conversion by supplying custom functions, named
# by the data type of the cells they are to convert (look at the `data_type`
# column).  If your custom functions aren't sufficient to avoid the need for
# coercion, then they will be overridden.
spatter(y, header,
        formatters = list(character = ~ toupper(.), numeric = as.complex))
#> # A tibble: 6 × 4
#>     row extra Age   Name    
#>   <int> <int> <cpl> <chr>   
#> 1     2    11   NA  MATILDA 
#> 2     2    12 1+0i  NA      
#> 3     3    13   NA  NICHOLAS
#> 4     3    14 3+0i  NA      
#> 5     4    15   NA  OLIVIA  
#> 6     4    16 5+0i  NA      
```
