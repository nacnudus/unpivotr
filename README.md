
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unpivotr

[![Travis-CI Build
Status](https://travis-ci.org/nacnudus/unpivotr.svg?branch=master)](https://travis-ci.org/nacnudus/unpivotr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/nacnudus/unpivotr?branch=master&svg=true)](https://ci.appveyor.com/project/nacnudus/unpivotr)
[![Cran
Status](http://www.r-pkg.org/badges/version/unpivotr)](https://CRAN.R-project.org/package=unpivotr)
[![Cran
Downloads](https://cranlogs.r-pkg.org/badges/unpivotr)](https://www.r-pkg.org/pkg/unpivotr)
[![codecov](https://codecov.io/github/nacnudus/unpivotr/coverage.svg?branch=master)](https://codecov.io/gh/nacnudus/unpivotr)

[unpivotr](https://github.com/nacnudus/unpivotr) deals with non-tabular
data, especially from spreadsheets. Use unpivotr when your source data
has any of these ‘features’:

  - Multi-headered hydra
  - Meaningful formatting
  - Headers anywhere but at the top of each column
  - Non-text headers e.g. dates
  - Other stuff around the table
  - Several similar tables in one sheet
  - Sentinel values
  - Superscript symbols
  - Meaningful comments
  - Nested HTML tables

If that list makes your blood boil, you’ll enjoy the function names.

  - `behead()` deals with multi-headered hydra tables one layer of
    headers at a time, working from the edge of the table inwards. It’s
    a bit like using `header = TRUE` in `read.csv()`, but because it’s a
    function, you can apply it to as many layers of headers as you need.
    You end up with all the headers in columns.
  - `spatter()` is like `tidyr::spread()` but preserves mixed data
    types. You get into a mixed-data-type situation by delaying type
    coercion until *after* the table is tidy (rather than before, like
    `read.csv()` et al). And yes, it usually follows `behead()`.

More positive, corrective functions:

  - `justify()` aligns column headers before `behead()`ing, and has
    deliberate moral overtones.
  - `enhead()` attaches a header to the body of the data, *a la*
    Frankenstein. The effect is the same as `behead()`, but is more
    powerful because you can choose exactly which header cells you want,
    paying attention to formatting (which `behead()` doesn’t
    understand).
  - `isolate_sentinels()` separates meaningful symbols like `"N/A"` or
    `"confidential"` from the rest of the data, giving them some time
    alone think about what they’ve done.
  - `partition()` takes a sheet with several tables on it, and slashes
    into pieces that each contain one table. You can then unpivot each
    table in turn with `purrr::map()` or similar.

## Make cells tidy

Unpivotr uses data where each cells is represented by one row in a
dataframe. Like this.

![Gif of tidyxl converting cells into a tidy representation of one row
per cell](./vignettes/tidy_xlsx.gif)

What can you do with tidy cells? The best places to start are:

  - [Spreadsheet Munging
    Strategies](https://nacnudus.github.io/spreadsheet-munging-strategies),
    a free, online cookbook using
    [tidyxl](https://github.com/nacnudus/tidyxl) and
    [unpivotr](https://github.com/nacnudus/unpivotr)
  - [Screencasts](https://www.youtube.com/watch?v=1sinC7wsS5U) on
    YouTube.
  - [Worked examples](https://github.com/nacnudus/ukfarm) on GitHub.

Otherwise the basic idea is:

1.  Read the data with a specialist tool.
      - For spreadsheets, use
        [tidyxl](https://nacnudus.github.io/tidyxl).
      - For plain text files, you might soon be able to use
        [readr](https://readr.tidyverse.org), but for now you’ll have to
        install a pull-request on that package with
        `devtools::install_github("tidyverse/readr#760")`.
      - For tables in html pages, use `unpivotr::tidy_html()`
      - For data frames, use `unpivotr::as_cells()` – this should be a
        last resort, because by the time the data is in a conventional
        data frame, it is often too late – formatting has been lost, and
        most data types have been coerced to strings.
2.  Either `behead()` straight away, else `dplyr::filter()` separately
    for the header cells and the data cells, and then recombine with
    `enhead()`.
3.  `spatter()` so that each column has one data type.

<!-- end list -->

``` r
library(unpivotr)
library(tidyverse)
#> ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   0.8.3     ✔ stringr 1.4.0
#> ✔ readr   1.3.1     ✔ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
x <- purpose$`up-left left-up`
x # A pivot table in a conventional data frame.  Four levels of headers, in two
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
  # rows and two columns.

y <- as_cells(x) # 'Tokenize' or 'melt' the data frame into one row per cell
y
#> # A tibble: 132 x 4
#>      row   col data_type chr              
#>    <int> <int> <chr>     <chr>            
#>  1     1     1 chr       <NA>             
#>  2     2     1 chr       <NA>             
#>  3     3     1 chr       Bachelor's degree
#>  4     4     1 chr       <NA>             
#>  5     5     1 chr       <NA>             
#>  6     6     1 chr       <NA>             
#>  7     7     1 chr       Certificate      
#>  8     8     1 chr       <NA>             
#>  9     9     1 chr       <NA>             
#> 10    10     1 chr       <NA>             
#> # … with 122 more rows

rectify(y) # useful for reviewing the melted form as though in a spreadsheet
#> # A tibble: 22 x 7
#>    `row/col` `1(A)`            `2(B)`  `3(C)` `4(D)` `5(E)` `6(F)`
#>        <int> <chr>             <chr>   <chr>  <chr>  <chr>  <chr> 
#>  1         1 <NA>              <NA>    Female <NA>   Male   <NA>  
#>  2         2 <NA>              <NA>    0 - 6  7 - 10 0 - 6  7 - 10
#>  3         3 Bachelor's degree 15 - 24 7000   27000  <NA>   13000 
#>  4         4 <NA>              25 - 44 12000  137000 9000   81000 
#>  5         5 <NA>              45 - 64 10000  64000  7000   66000 
#>  6         6 <NA>              65+     <NA>   18000  7000   17000 
#>  7         7 Certificate       15 - 24 29000  161000 30000  190000
#>  8         8 <NA>              25 - 44 34000  179000 31000  219000
#>  9         9 <NA>              45 - 64 30000  210000 23000  199000
#> 10        10 <NA>              65+     12000  77000  8000   107000
#> # … with 12 more rows

y %>%
  behead("up-left", "sex") %>%               # Strip headers
  behead("up", "life-satisfication") %>%  # one
  behead("left-up", "qualification") %>%     # by
  behead("left", "age-band") %>%            # one.
  select(-row, -col, -data_type, count = chr) %>% # cleanup
  mutate(count = as.integer(count))
#> # A tibble: 80 x 5
#>     count sex    `life-satisfication` qualification     `age-band`
#>     <int> <chr>  <chr>                <chr>             <chr>     
#>  1   7000 Female 0 - 6                Bachelor's degree 15 - 24   
#>  2  12000 Female 0 - 6                Bachelor's degree 25 - 44   
#>  3  10000 Female 0 - 6                Bachelor's degree 45 - 64   
#>  4     NA Female 0 - 6                Bachelor's degree 65+       
#>  5  27000 Female 7 - 10               Bachelor's degree 15 - 24   
#>  6 137000 Female 7 - 10               Bachelor's degree 25 - 44   
#>  7  64000 Female 7 - 10               Bachelor's degree 45 - 64   
#>  8  18000 Female 7 - 10               Bachelor's degree 65+       
#>  9     NA Male   0 - 6                Bachelor's degree 15 - 24   
#> 10   9000 Male   0 - 6                Bachelor's degree 25 - 44   
#> # … with 70 more rows
```

Note the compass directions in the code above, which hint to `behead()`
where to find the header cell for each data cell.

  - `"up-left"` means the header (`Female`, `Male`) is positioned up and
    to the left of the columns of data cells it describes.
  - `"up"` means the header (`0 - 6`, `7 - 10`) is positioned directly
    above the columns of data cells it describes.
  - `"left-up"` means the header (`Bachelor's degree`, `Certificate`,
    etc.) is positioned to the left and upwards of the rows of data
    cells it describes.
  - `"left"` means the header (`15 - 24`, `25 - 44`, etc.) is positioned
    directly to the left of the rows of data cells it describes.

## Installation

``` r
# install.packages("devtools") # If you don't already have devtools
devtools::install_github("nacnudus/unpivotr", build_vignettes = TRUE)
```

The version 0.4.0 release had somee breaking changes. See `NEWS.md` for
details. The previous version can be installed as follow:

``` r
devtools::install_version("unpivotr", version = "0.3.1", repos = "http://cran.us.r-project.org")
```

## Similar projects

[unpivotr](https://github.com/nacnudus/unpivotr) is inspired by
[Databaker](https://github.com/sensiblecodeio/databaker), a
collaboration between the [United Kingdom Office of National
Statistics](http://ons.gov.uk) and [The Sensible Code
Company](https://sensiblecode.io/).
[unpivotr](https://github.com/nacnudus/unpivotr).

[jailbreaker](https://github.com/rsheets/jailbreakr) attempts to extract
non-tabular data from spreadsheets into tabular structures automatically
via some clever algorithms.
[unpivotr](https://github.com/nacnudus/unpivotr) differs by being less
magic, and equipping you to express what you want to do.
>>>>>>> master
