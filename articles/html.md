# HTML Tables

This vignette for the [unpivotr](https://github.com/nacnudus/unpivotr)
package demonstrates unpivoting html tables of various kinds.

The HTML files are in the package directory at
`system.file("extdata", c("rowspan.html", "colspan.html", "nested.html"), package = "unpivotr")`.

``` r

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r

library(rvest)
library(htmltools)
library(unpivotr)
```

## Rowspan and colspan examples

If a table has cells merged across rows or columns (or both), then
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
does not attempt to fill the cell contents across the rows or columns.
This is different from other packages, e.g. `rvest`. However, if merged
cells cause a table not to be square, then
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
pads the missing cells with blanks.

### Rowspan

``` r

rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
includeHTML(rowspan)
```

HTML table with rowspan

  

| Header (1:2, 1) | Header (1, 2) |
|-----------------|---------------|
| cell (2, 2)     |               |

  

``` r

# rvest
rowspan %>%
  read_html() %>%
  html_table()
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##   `Header (1:2, 1)` `Header (1, 2)`
    ##   <chr>             <chr>          
    ## 1 Header (1:2, 1)   cell (2, 2)

``` r

# unpivotr
rowspan %>%
  read_html() %>%
  as_cells()
```

    ## [[1]]
    ## # A tibble: 4 × 4
    ##     row   col data_type html                                    
    ##   <int> <int> <chr>     <chr>                                   
    ## 1     1     1 html      "<th rowspan=\"2\">Header (1:2, 1)</th>"
    ## 2     2     1 html       NA                                     
    ## 3     1     2 html      "<th>Header (1, 2)</th>"                
    ## 4     2     2 html      "<td>cell (2, 2)</td>"

### Colspan

``` r

colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
includeHTML(colspan)
```

HTML table with colspan

  

| Header (1, 1:2) |             |
|-----------------|-------------|
| cell (2, 1)     | cell (2, 2) |

  

``` r

# rvest
colspan %>%
  read_html() %>%
  html_table()
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##   `Header (1, 1:2)` `Header (1, 1:2)`
    ##   <chr>             <chr>            
    ## 1 cell (2, 1)       cell (2, 2)

``` r

# unpivotr
colspan %>%
  read_html() %>%
  as_cells()
```

    ## [[1]]
    ## # A tibble: 4 × 4
    ##     row   col data_type html                                    
    ##   <int> <int> <chr>     <chr>                                   
    ## 1     1     1 html      "<th colspan=\"2\">Header (1, 1:2)</th>"
    ## 2     2     1 html      "<td>cell (2, 1)</td>"                  
    ## 3     1     2 html       NA                                     
    ## 4     2     2 html      "<td>cell (2, 2)</td>"

### Both rowspan and colspan: non-square

``` r

rowandcolspan <- system.file("extdata",
                             "row-and-colspan.html",
                             package = "unpivotr")
includeHTML(rowandcolspan)
```

HTML table with colspan

  

| Header (1:2, 1:2) |             | Header (2, 3) |
|-------------------|-------------|---------------|
| cell (3, 1)       | cell (3, 2) | cell (3, 3)   |

  

``` r

# rvest
rowandcolspan %>%
  read_html() %>%
  html_table()
```

    ## [[1]]
    ## # A tibble: 1 × 5
    ##   `Header (1:2, 1:2)` `Header (1:2, 1:2)` `Header (2, 3)` ``          ``        
    ##   <chr>               <chr>               <chr>           <chr>       <chr>     
    ## 1 Header (1:2, 1:2)   Header (1:2, 1:2)   cell (3, 1)     cell (3, 2) cell (3, …

``` r

# unpivotr
rowandcolspan %>%
  read_html() %>%
  as_cells()
```

    ## [[1]]
    ## # A tibble: 10 × 4
    ##      row   col data_type html                                                   
    ##    <int> <int> <chr>     <chr>                                                  
    ##  1     1     1 html      "<th colspan=\"2\" rowspan=\"2\">Header (1:2, 1:2)</th…
    ##  2     2     1 html       NA                                                    
    ##  3     1     2 html       NA                                                    
    ##  4     2     2 html       NA                                                    
    ##  5     1     3 html      "<th>Header (2, 3)</th>"                               
    ##  6     2     3 html      "<td>cell (3, 1)</td>"                                 
    ##  7     1     4 html       NA                                                    
    ##  8     2     4 html      "<td>cell (3, 2)</td>"                                 
    ##  9     1     5 html       NA                                                    
    ## 10     2     5 html      "<td>cell (3, 3)</td>"

## Nested example

[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
never descends into cells. If there is a table inside a cell, then to
parse that table use `html_table` again on that cell.

``` r

nested <- system.file("extdata", "nested.html", package = "unpivotr")
includeHTML(nested)
```

Nested HTML table

  

[TABLE]

  

``` r

# rvest parses both tables
nested %>%
  read_html() %>%
  html_table()
```

    ## [[1]]
    ## # A tibble: 3 × 6
    ##   `Header (1, 1)`     `Header (1, 2)`                    ``    ``    ``    ``   
    ##   <chr>               <chr>                              <chr> <chr> <chr> <chr>
    ## 1 cell (2, 1)         "Header (2, 2)(1, 1)\n           … Head… Head… cell… cell…
    ## 2 Header (2, 2)(1, 1) "Header (2, 2)(1, 2)"              NA    NA    NA    NA   
    ## 3 cell (2, 2)(2, 1)   "cell (2, 2)(2, 1)"                NA    NA    NA    NA   
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##   `Header (2, 2)(1, 1)` `Header (2, 2)(1, 2)`
    ##   <chr>                 <chr>                
    ## 1 cell (2, 2)(2, 1)     cell (2, 2)(2, 1)

``` r

# unpivotr
x <-
  nested %>%
  read_html() %>%
  as_cells() %>%
  .[[1]]
x
```

    ## # A tibble: 4 × 4
    ##     row   col data_type html                                                    
    ##   <int> <int> <chr>     <chr>                                                   
    ## 1     1     1 html      "<th>Header (1, 1)</th>"                                
    ## 2     2     1 html      "<td>cell (2, 1)</td>"                                  
    ## 3     1     2 html      "<th>Header (1, 2)</th>"                                
    ## 4     2     2 html      "<td>\n          <table>\n<tr>\n<th>Header (2, 2)(1, 1)…

``` r

# The html of the table inside a cell
cell <-
  x %>%
  dplyr::filter(row == 2, col == 2) %>%
  .$html
cell
```

    ## [1] "<td>\n          <table>\n<tr>\n<th>Header (2, 2)(1, 1)</th>\n              <th>Header (2, 2)(1, 2)</th>\n            </tr>\n<tr>\n<td>cell (2, 2)(2, 1)</td>\n              <td>cell (2, 2)(2, 1)</td>\n            </tr>\n</table>\n</td>"

``` r

# Parsing the table inside the cell
cell %>%
  read_html() %>%
  as_cells()
```

    ## [[1]]
    ## # A tibble: 4 × 4
    ##     row   col data_type html                        
    ##   <int> <int> <chr>     <chr>                       
    ## 1     1     1 html      <th>Header (2, 2)(1, 1)</th>
    ## 2     2     1 html      <td>cell (2, 2)(2, 1)</td>  
    ## 3     1     2 html      <th>Header (2, 2)(1, 2)</th>
    ## 4     2     2 html      <td>cell (2, 2)(2, 1)</td>

## URL example

A motivation for using
[`unpivotr::as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
is that it extracts more than just text – it can extract whatever part
of the HTML you need.

Here, we extract URLs.

``` r

urls <- system.file("extdata", "url.html", package = "unpivotr")
includeHTML(urls)
```

HTML table with rowspan

  

[TABLE]

  

``` r

cell_url <- function(x) {
  if (is.na(x)) return(NA)
  x %>%
    read_html %>%
    html_nodes("a") %>%
    html_attr("href")
}

cell_text <- function(x) {
  if (is.na(x)) return(NA)
  x %>%
    read_html %>%
    html_nodes("a") %>%
    html_text()
}

urls %>%
  read_html() %>%
  as_cells() %>%
  .[[1]] %>%
  mutate(text = purrr::map(html, cell_text),
         url = purrr::map(html, cell_url)) %>%
  tidyr::unnest(text, url)
```

    ## Warning: `unnest()` has a new interface. See `?unnest` for details.
    ## ℹ Try `df |> unnest(c(text, url))`, with `mutate()` if needed.

    ## # A tibble: 8 × 6
    ##     row   col data_type html                                         text  url  
    ##   <int> <int> <chr>     <chr>                                        <chr> <chr>
    ## 1     1     1 html      "<td colspan=\"2\">\n<a href=\"https://www.… Scra… http…
    ## 2     1     1 html      "<td colspan=\"2\">\n<a href=\"https://www.… HTML. http…
    ## 3     2     1 html      "<td><a href=\"https://cran.r-project.org/\… Sweet http…
    ## 4     1     2 html       NA                                          NA    NA   
    ## 5     2     2 html      "<td><a href=\"https://cran.r-project.org/p… as?   http…
    ## 6     1     3 html       NA                                          NA    NA   
    ## 7     2     3 html      "<td>\n<a href=\"https://cran.r-project.org… Yeah, http…
    ## 8     2     3 html      "<td>\n<a href=\"https://cran.r-project.org… righ… http…
