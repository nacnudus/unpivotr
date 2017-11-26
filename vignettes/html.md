---
title: "HTML Tables"
author: "Duncan Garmonsway"
date: "2017-11-13"
output:
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{Compass Directions}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

This vignette for the [unpivotr](https://github.com/nacnudus/unpivotr) package
demonstrates unpivoting html tables of various kinds.

The HTML files are in the package directory at `system.file("extdata",
c("rowspan.html", "colspan.html", "nested.html"), package = "unpivotr")`.


```r
library(dplyr)
library(rvest)
library(htmltools)
library(unpivotr)
```

## Rowspan and colspan examples

If a table has cells merged across rows or columns (or both), then `tidy_table`
does not attempt to fill the cell contents across the rows or columns.  This is
different from other packages, e.g. `rvest`.  However, if merged cells cause a
table not to be square, then `tidy_table` pads the missing cells with blanks.

### Rowspan


```r
rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
includeHTML(rowspan)
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <title>HTML table with rowspan</title>
    <style>
      table {
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid;
        padding: 10px;
      }
    </style>
  </head>
  <body>
    <br/>
    <table>
      <tr>
        <th rowspan="2">Header (1:2, 1)</th>
        <th>Header (1, 2)</th>
      </tr>
      <tr>
        <td>cell (2, 2)</td>
      </tr>
    </table>
  </body>
</html>
<br/><!--/html_preserve-->

```r
# rvest
rowspan %>%
  read_html() %>%
  html_table()
```

```
## [[1]]
##   Header (1:2, 1) Header (1, 2)
## 1 Header (1:2, 1)   cell (2, 2)
```

```r
# unpivotr
rowspan %>%
  read_html() %>%
  tidy_table()
```

```
## [[1]]
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```

### Colspan


```r
colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
includeHTML(colspan)
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <title>HTML table with colspan</title>
    <style>
      table {
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid;
        padding: 10px;
      }
    </style>
  </head>
  <br/>
  <body>
    <table>
      <tr>
        <th colspan="2">Header (1, 1:2)</th>
      </tr>
      <tr>
        <td>cell (2, 1)</td>
        <td>cell (2, 2)</td>
      </tr>
    </table>
  </body>
</html>
<div/>
  <br/><!--/html_preserve-->

```r
# rvest
colspan %>%
  read_html() %>%
  html_table()
```

```
## [[1]]
##   Header (1, 1:2) Header (1, 1:2)
## 1     cell (2, 1)     cell (2, 2)
```

```r
# unpivotr
colspan %>%
  read_html() %>%
  tidy_table()
```

```
## [[1]]
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```

### Both rowspan and colspan: non-square


```r
rowandcolspan <- system.file("extdata",
                             "row-and-colspan.html",
                             package = "unpivotr")
includeHTML(rowandcolspan)
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <title>HTML table with colspan</title>
    <style>
      table {
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid;
        padding: 10px;
      }
    </style>
  </head>
  <body>
    <br/>
    <table>
      <tr>
        <th colspan="2" rowspan="2">Header (1:2, 1:2)</th>
        <th>Header (2, 3)</th>
      </tr>
      <tr>
        <td>cell (3, 1)</td>
        <td>cell (3, 2)</td>
        <td>cell (3, 3)</td>
      </tr>
    </table>
  </body>
</html>
<br/><!--/html_preserve-->

```r
# rvest
rowandcolspan %>%
  read_html() %>%
  html_table()
```

```
## [[1]]
##   Header (1:2, 1:2) Header (1:2, 1:2) Header (2, 3)
## 1 Header (1:2, 1:2) Header (1:2, 1:2)   cell (3, 1)
```

```r
# unpivotr
rowandcolspan %>%
  read_html() %>%
  tidy_table()
```

```
## [[1]]
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```

## Nested example

`tidy_table` never descends into cells.  If there is a table inside a cell, then
to parse that table use `html_table` again on that cell.


```r
nested <- system.file("extdata", "nested.html", package = "unpivotr")
includeHTML(nested)
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <title>Nested HTML table</title>
    <style>
      table {
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid;
        padding: 10px;
      }
    </style>
  </head>
  <body>
    <br/>
    <table>
      <tr>
        <th>Header (1, 1)</th>
        <th>Header (1, 2)</th>
      </tr>
      <tr>
        <td>cell (2, 1)</td>
        <td>
          <table>
            <tr>
              <th>Header (2, 2)(1, 1)</th>
              <th>Header (2, 2)(1, 2)</th>
            </tr>
            <tr>
              <td>cell (2, 2)(2, 1)</td>
              <td>cell (2, 2)(2, 1)</td>
            </tr>
          </table>
        </td>
      </tr>
    </table>
  </body>
</html>
<br/><!--/html_preserve-->

```r
# rvest parses both tables
nested %>%
  read_html() %>%
  html_table(fill = TRUE)
```

```
## [[1]]
##         Header (1, 1)
## 1         cell (2, 1)
## 2 Header (2, 2)(1, 1)
## 3   cell (2, 2)(2, 1)
##                                                                                                            Header (1, 2)
## 1 Header (2, 2)(1, 1)\n              Header (2, 2)(1, 2)\n            cell (2, 2)(2, 1)\n              cell (2, 2)(2, 1)
## 2                                                                                                    Header (2, 2)(1, 2)
## 3                                                                                                      cell (2, 2)(2, 1)
##                    NA                  NA                NA
## 1 Header (2, 2)(1, 1) Header (2, 2)(1, 2) cell (2, 2)(2, 1)
## 2                <NA>                <NA>              <NA>
## 3                <NA>                <NA>              <NA>
##                  NA
## 1 cell (2, 2)(2, 1)
## 2              <NA>
## 3              <NA>
## 
## [[2]]
##   Header (2, 2)(1, 1) Header (2, 2)(1, 2)
## 1   cell (2, 2)(2, 1)   cell (2, 2)(2, 1)
```

```r
# unpivotr
x <-
  nested %>%
  read_html() %>%
  tidy_table() %>%
  .[[1]]
x
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```

```r
# The html of the table inside a cell
cell <-
  x %>%
  filter(row == 2, col == 2) %>%
  .$html
cell
```

```
## [1] "<td>\n          <table>\n<tr>\n<th>Header (2, 2)(1, 1)</th>\n              <th>Header (2, 2)(1, 2)</th>\n            </tr>\n<tr>\n<td>cell (2, 2)(2, 1)</td>\n              <td>cell (2, 2)(2, 1)</td>\n            </tr>\n</table>\n</td>"
```

```r
# Parsing the table inside the cell
cell %>%
  read_html() %>%
  tidy_table()
```

```
## [[1]]
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```

## URL example

A motivation for using `unpivotr::tidy_table()` is that it extracts more than
just text -- it can extract whatever part of the HTML you need.

Here, we extract URLs.


```r
urls <- system.file("extdata", "url.html", package = "unpivotr")
includeHTML(urls)
```

<!--html_preserve--><!DOCTYPE html>
<html>
  <head>
    <title>HTML table with rowspan</title>
    <style>
      table {
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid;
        padding: 10px;
      }
    </style>
  </head>
  <body>
    <br/>
    <table>
      <tr>
        <td colspan="2"><a href="example1.co.nz">Scraping</a> <a href="example2.co.nz">HTML.</a></td>
      </tr>
      <tr>
        <td><a href="example3.co.nz">Sweet</a></td>
        <td><a href="example4.co.nz">as?</a></td>
          <td><a href="example5.co.nz">Yeah,</a><br/><a href="http://www.tui.co.nz/">right.</a></td>
      </tr>
    </table>
  </body>
</html>
<br/><!--/html_preserve-->

```r
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
  tidy_table() %>%
  .[[1]] %>%
  mutate(text = purrr::map(html, cell_text),
         url = purrr::map(html, cell_url)) %>%
  tidyr::unnest(text, url)
```

```
## Error: 'utf8_width' is not an exported object from 'namespace:utf8'
```
