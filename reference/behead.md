# Strip a level of headers from a pivot table

`behead()` takes one level of headers from a pivot table and makes it
part of the data. Think of it like
[`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html),
except that it works when there is more than one row of headers (or more
than one column of row-headers), and it only works on tables that have
first come through
[`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
or
[`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html).

## Usage

``` r
behead(
  cells,
  direction,
  name,
  values = NULL,
  types = data_type,
  formatters = list(),
  drop_na = TRUE
)

behead_if(
  cells,
  ...,
  direction,
  name,
  values = NULL,
  types = data_type,
  formatters = list(),
  drop_na = TRUE
)
```

## Arguments

- cells:

  Data frame. The cells of a pivot table, usually the output of
  [`as_cells()`](https://nacnudus.github.io/unpivotr/reference/as_cells.md)
  or
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html),
  or of a subsequent operation on those outputs.

- direction:

  The direction between a data cell and its header, one of `"up"`,
  `"right"`, `"down"`, `"left"`, `"up-left"`, `"up-right"`,
  `"right-up"`, `"right-down"`, `"down-right"`, `"down-left"`,
  `"left-down"`, `"left-up"`. See
  [`?direction`](https://nacnudus.github.io/unpivotr/reference/direction.md).
  `"up-ish"`, `"down-ish"`, `"left-ish"` and `"right-ish"` aren't
  available because they require certain ambiguities that are better
  handled by using
  [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  directly rather than via `behead()`.

- name:

  A name to give the new column that will be created, e.g. `"location"`
  if the headers are locations. Quoted (`"location"`, not `location`)
  because it doesn't refer to an actual object.

- values:

  Optional. The column of `cells` to use as the values of each header.
  Given as a bare variable name. If omitted (the default), the `types`
  argument will be used instead.

- types:

  The name of the column that names the data type of each cell. Usually
  called `data_types` (the default), this is a character column that
  names the other columns in `cells` that contain the values of each
  cell. E.g. a cell with a character value will have `"character"` in
  this column. Unquoted(`data_types`, not `"data_types"`) because it
  refers to an actual object.

- formatters:

  A named list of functions for formatting each data type in a set of
  headers of mixed data types, e.g. when some headers are dates and
  others are characters. These can be given as `character = toupper` or
  `character = ~ toupper(.x)`, similar to
  [purrr::map](https://purrr.tidyverse.org/reference/map.html).

- drop_na:

  logical Whether to filter out headers that have `NA` in the `value`
  column. Default: `TRUE`. This can happen with the output of
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html),
  when an empty cell exists because it has formatting applied to it, but
  should be ignored.

- ...:

  Passed to
  [dplyr::filter](https://dplyr.tidyverse.org/reference/filter.html).
  logical predicates defined in terms of the variables in `.data`.
  Multiple conditions are combined with `&`. Only rows where the
  condition evaluates to `TRUE` are kept.

  The arguments in `...` are automatically
  [quoted](https://rlang.r-lib.org/reference/defusing-advanced.html) and
  [evaluated](https://rlang.r-lib.org/reference/eval_tidy.html) in the
  context of the data frame. They support
  [unquoting](https://rlang.r-lib.org/reference/topic-inject.html) and
  splicing. See the dplyr `vignette("programming")` for an introduction
  to these concepts.

## Value

A data frame

## Examples

``` r
# A simple table with a row of headers
(x <- data.frame(a = 1:2, b = 3:4))
#>   a b
#> 1 1 3
#> 2 2 4

# Make a tidy representation of each cell
(cells <- as_cells(x, col_names = TRUE))
#> # A tibble: 6 × 5
#>     row   col data_type chr     int
#>   <int> <int> <chr>     <chr> <int>
#> 1     1     1 chr       a        NA
#> 2     2     1 int       NA        1
#> 3     3     1 int       NA        2
#> 4     1     2 chr       b        NA
#> 5     2     2 int       NA        3
#> 6     3     2 int       NA        4

# Strip the cells in row 1 (the original headers) and use them as data
behead(cells, "N", foo)
#> # A tibble: 4 × 6
#>     row   col data_type chr     int foo  
#>   <int> <int> <chr>     <chr> <int> <chr>
#> 1     2     1 int       NA        1 a    
#> 2     3     1 int       NA        2 a    
#> 3     2     2 int       NA        3 b    
#> 4     3     2 int       NA        4 b    

# More complex example: pivot table with several layers of headers
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
head(cells)
#> # A tibble: 6 × 4
#>     row   col data_type chr              
#>   <int> <int> <chr>     <chr>            
#> 1     1     1 chr       NA               
#> 2     2     1 chr       NA               
#> 3     3     1 chr       Bachelor's degree
#> 4     4     1 chr       NA               
#> 5     5     1 chr       NA               
#> 6     6     1 chr       NA               
tail(cells)
#> # A tibble: 6 × 4
#>     row   col data_type chr  
#>   <int> <int> <chr>     <chr>
#> 1    17     6 chr       75000
#> 2    18     6 chr       66000
#> 3    19     6 chr       NA   
#> 4    20     6 chr       60000
#> 5    21     6 chr       68000
#> 6    22     6 chr       18000

# Strip the headers and make them into data
tidy <-
  cells %>%
  behead("up-left", Sex) %>%
  behead("up", `Sense of purpose`) %>%
  behead("left-up", `Highest qualification`) %>%
  behead("left", `Age group (Life-stages)`) %>%
  dplyr::mutate(count = as.integer(chr)) %>%
  dplyr::select(-row, -col, -data_type, -chr)
head(tidy)
#> # A tibble: 6 × 5
#>   Sex    `Sense of purpose` Highest qualificatio…¹ Age group (Life-stag…²  count
#>   <chr>  <chr>              <chr>                  <chr>                   <int>
#> 1 Female 0 - 6              Bachelor's degree      15 - 24                  7000
#> 2 Female 0 - 6              Bachelor's degree      25 - 44                 12000
#> 3 Female 0 - 6              Bachelor's degree      45 - 64                 10000
#> 4 Female 0 - 6              Bachelor's degree      65+                        NA
#> 5 Female 7 - 10             Bachelor's degree      15 - 24                 27000
#> 6 Female 7 - 10             Bachelor's degree      25 - 44                137000
#> # ℹ abbreviated names: ¹​`Highest qualification`, ²​`Age group (Life-stages)`

# Check against the provided 'tidy' version of the data.
dplyr::anti_join(tidy, purpose$Tidy)
#> Joining with `by = join_by(Sex, `Sense of purpose`, `Highest qualification`,
#> `Age group (Life-stages)`)`
#> # A tibble: 1 × 5
#>   Sex   `Sense of purpose` `Highest qualification`  Age group (Life-stag…¹ count
#>   <chr> <chr>              <chr>                    <chr>                  <int>
#> 1 Male  0 - 6              Postgraduate qualificat… 15 - 24                   NA
#> # ℹ abbreviated name: ¹​`Age group (Life-stages)`

# The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
# postgraduate qualification and a sense of purpose between 0 and 6.  That
# seems to have been an oversight by Statistics New Zealand.

cells <- tibble::tribble(
       ~X1, ~adult, ~juvenile,
    "LION",    855,       677,
    "male",    496,       322,
  "female",    359,       355,
   "TIGER",    690,       324,
    "male",    381,       222,
  "female",    309,       102
  )
cells <- as_cells(cells, col_names = TRUE)

cells %>%
  behead_if(chr == toupper(chr), direction = "left-up", name = "species") %>%
  behead("left", "sex") %>%
  behead("up", "age") %>%
  dplyr::select(species, sex, age, population = dbl)
#> # A tibble: 12 × 4
#>    species sex    age      population
#>    <chr>   <chr>  <chr>         <dbl>
#>  1 LION    NA     adult           855
#>  2 LION    male   adult           496
#>  3 LION    female adult           359
#>  4 LION    NA     juvenile        677
#>  5 LION    male   juvenile        322
#>  6 LION    female juvenile        355
#>  7 TIGER   NA     adult           690
#>  8 TIGER   male   adult           381
#>  9 TIGER   female adult           309
#> 10 TIGER   NA     juvenile        324
#> 11 TIGER   male   juvenile        222
#> 12 TIGER   female juvenile        102
```
