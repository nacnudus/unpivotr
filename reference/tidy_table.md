# Tokenize data frames into a tidy 'melted' structure

`tidy_table()` will be deprecated. Use
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
instead.

For certain non-rectangular data formats, it can be useful to parse the
data into a melted format where each row represents a single token.

Data frames represent data in a tabular structure. `tidy_table` takes
the row and column position of each 'cell', and returns that information
in a new data frame, alongside the content and type of each cell.

This makes it easier to deal with complex or non-tabular data (e.g.
pivot tables) that have been imported into R as data frames. Once they
have been 'melted' by `tidy_table()`, you can use functions like
[`behead()`](https://nacnudus.github.io/unpivotr/reference/behead.md)
and
[`spatter()`](https://nacnudus.github.io/unpivotr/reference/spatter.md)
to reshape them into conventional, tidy, unpivoted structures.

For HTML tables, the content of each cell is returned as a standalone
HTML string that can be further parsed with tools such as the rvest
package. This is particularly useful when an HTML cell itself contains
an HTML table, or contains both text and a URL. If the HTML itself is
poorly formatted, try passing it through the
[htmltidy](https://CRAN.R-project.org/package=htmltidy) package first.

This is an S3 generic.

## Usage

``` r
tidy_table(x, row_names = FALSE, col_names = FALSE)
```

## Arguments

- x:

  A data.frame or an HTML document

- row_names:

  Whether to treat the row names as cells, Default: FALSE

- col_names:

  Whether to treat the column names as cells, Default: FALSE

## Value

A data.frame with the following columns:

- `row` and `col` (integer) giving the original position of the 'cells'

- any relevant columns for cell values in their original types: `chr`,
  `cpl`, `dbl`, `fct`, `int`, `lgl`, `list`, and `ord`

- `data_type` to specify for each cell which of the above columns (`chr`
  etc.) the value is in.

The columns `fct` and `ord` are, like `list`, list-columns (each element
is independent) to avoid factor levels clashing. For HTML tables, the
column `html` gives the HTML string of the original cell.

Row and column names, when present and required by `row_names = TRUE` or
`col_names = TRUE`, are treated as though they were cells in the table,
and they appear in the `chr` column.

## Examples

``` r
x <- data.frame(a = c(10, 20),
                b = c("foo", "bar"),
                stringsAsFactors = FALSE)
x
#>    a   b
#> 1 10 foo
#> 2 20 bar
tidy_table(x)
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> # A tibble: 4 × 5
#>     row   col data_type chr     dbl
#>   <int> <int> <chr>     <chr> <dbl>
#> 1     1     1 dbl       NA       10
#> 2     2     1 dbl       NA       20
#> 3     1     2 chr       foo      NA
#> 4     2     2 chr       bar      NA
tidy_table(x, row_names = TRUE)
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> # A tibble: 6 × 5
#>     row   col data_type chr     dbl
#>   <int> <int> <chr>     <chr> <dbl>
#> 1     1     1 chr       1        NA
#> 2     2     1 chr       2        NA
#> 3     1     2 dbl       NA       10
#> 4     2     2 dbl       NA       20
#> 5     1     3 chr       foo      NA
#> 6     2     3 chr       bar      NA
tidy_table(x, col_names = TRUE)
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> # A tibble: 6 × 5
#>     row   col data_type chr     dbl
#>   <int> <int> <chr>     <chr> <dbl>
#> 1     1     1 chr       a        NA
#> 2     2     1 dbl       NA       10
#> 3     3     1 dbl       NA       20
#> 4     1     2 chr       b        NA
#> 5     2     2 chr       foo      NA
#> 6     3     2 chr       bar      NA

# 'list' columns are undisturbed
y <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE)
y$b <- list(1:2, 3:4)
y
#>   a    b
#> 1 a 1, 2
#> 2 b 3, 4
tidy_table(y)
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> # A tibble: 4 × 5
#>     row   col data_type chr   list     
#>   <int> <int> <chr>     <chr> <list>   
#> 1     1     1 chr       a     <NULL>   
#> 2     2     1 chr       b     <NULL>   
#> 3     1     2 list      NA    <int [2]>
#> 4     2     2 list      NA    <int [2]>

# Factors are preserved by being wrapped in lists so that their levels don't
# conflict.  Blanks are NULLs.
z <- data.frame(x = factor(c("a", "b")),
                y = factor(c("c", "d"), ordered = TRUE))
tidy_table(z)
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> # A tibble: 4 × 5
#>     row   col data_type fct       ord      
#>   <int> <int> <chr>     <list>    <list>   
#> 1     1     1 fct       <fct [1]> <NULL>   
#> 2     2     1 fct       <fct [1]> <NULL>   
#> 3     1     2 ord       <NULL>    <ord [1]>
#> 4     2     2 ord       <NULL>    <ord [1]>
tidy_table(z)$fct
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> [[1]]
#> [1] a
#> Levels: a b
#> 
#> [[2]]
#> [1] b
#> Levels: a b
#> 
#> [[3]]
#> NULL
#> 
#> [[4]]
#> NULL
#> 
tidy_table(z)$ord
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> [1] c
#> Levels: c < d
#> 
#> [[4]]
#> [1] d
#> Levels: c < d
#> 

# HTML tables can be extracted from the output of xml2::read_html().  These
# are returned as a list of tables, similar to rvest::html_table().  The
# value of each cell is its standalone HTML string, which can contain
# anything -- even another table.

colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
nested <- system.file("extdata", "nested.html", package = "unpivotr")

if (FALSE) { # \dontrun{
browseURL(colspan)
browseURL(rowspan)
browseURL(nestedspan)
} # }

tidy_table(xml2::read_html(colspan))
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> [[1]]
#> # A tibble: 4 × 4
#>     row   col data_type html                                    
#>   <int> <int> <chr>     <chr>                                   
#> 1     1     1 html      "<th colspan=\"2\">Header (1, 1:2)</th>"
#> 2     2     1 html      "<td>cell (2, 1)</td>"                  
#> 3     1     2 html       NA                                     
#> 4     2     2 html      "<td>cell (2, 2)</td>"                  
#> 
tidy_table(xml2::read_html(rowspan))
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> [[1]]
#> # A tibble: 4 × 4
#>     row   col data_type html                                    
#>   <int> <int> <chr>     <chr>                                   
#> 1     1     1 html      "<th rowspan=\"2\">Header (1:2, 1)</th>"
#> 2     2     1 html       NA                                     
#> 3     1     2 html      "<th>Header (1, 2)</th>"                
#> 4     2     2 html      "<td>cell (2, 2)</td>"                  
#> 
tidy_table(xml2::read_html(nested))
#> tidy_table() will be deprecated.  Use as_cells() instead.
#> [[1]]
#> # A tibble: 4 × 4
#>     row   col data_type html                                                    
#>   <int> <int> <chr>     <chr>                                                   
#> 1     1     1 html      "<th>Header (1, 1)</th>"                                
#> 2     2     1 html      "<td>cell (2, 1)</td>"                                  
#> 3     1     2 html      "<th>Header (1, 2)</th>"                                
#> 4     2     2 html      "<td>\n          <table>\n<tr>\n<th>Header (2, 2)(1, 1)…
#> 
```
