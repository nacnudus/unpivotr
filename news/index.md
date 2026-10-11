# Changelog

## unpivotr 0.6.4

CRAN release: 2024-11-30

- Make tests compatible with recent versions of tzdata (CRAN)

## unpivotr 0.6.3

CRAN release: 2023-01-22

- Make compatible with dplyr v1.1.0
  ([\#58](https://github.com/nacnudus/unpivotr/issues/58)
  [@DavisVaughan](https://github.com/DavisVaughan))
- Implement `as_cells.matrix()`
  ([\#52](https://github.com/nacnudus/unpivotr/issues/52)
  [@billdenney](https://github.com/billdenney))
- Allow behead() to work on grouped data
  ([\#46](https://github.com/nacnudus/unpivotr/issues/46)
  [@billdenney](https://github.com/billdenney))
- Add as_cells.matrix
  ([\#41](https://github.com/nacnudus/unpivotr/issues/41)
  [@billdenney](https://github.com/billdenney))

## unpivotr 0.6.2

CRAN release: 2021-08-22

- Fix for upstream changes in rlang
  ([\#49](https://github.com/nacnudus/unpivotr/issues/49) thanks
  [@lionel-](https://github.com/lionel-))

## unpivotr 0.6.1

CRAN release: 2020-08-03

- Fix concatenation of `NULL` `POSIXct` (dates)
  ([\#39](https://github.com/nacnudus/unpivotr/issues/39)
  [@romainfrancois](https://github.com/romainfrancois))

## unpivotr 0.6.0

CRAN release: 2020-05-08

- Change `"NNW"` to `"up-left"` etc. Compass directions still work, but
  the built-in dataset `purpose` has different names and documentation
  will gradually change to the new terms.
- Patches for upcoming upstream changes

## unpivotr 0.5.1

CRAN release: 2019-03-30

- Patch for tibble `.name_repair` compatibility
  ([\#2144](https://github.com/nacnudus/unpivotr/issues/2144)
  [@krlmlr](https://github.com/krlmlr))
- Patch to switch to `tidyselect` from `dplyr` functions.

## unpivotr 0.5.0

CRAN release: 2019-01-02

### New features

- [`behead_if()`](https://nacnudus.github.io/unpivotr/reference/behead.md)
  is for tiered headers within the same row or column. It takes filter
  functions similarly to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  to decide which cells to treat as headers, and can be applied more
  than once to the same row or column of headers until every tier has
  been dealt with.
- [`merge_rows()`](https://nacnudus.github.io/unpivotr/reference/merge_cells.md)
  and
  [`merge_cols()`](https://nacnudus.github.io/unpivotr/reference/merge_cells.md)
  combines header text when it is split over multiple cells.

### Other changes

- [`behead()`](https://nacnudus.github.io/unpivotr/reference/behead.md),
  [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md),
  [`partition()`](https://nacnudus.github.io/unpivotr/reference/partition.md)
  and
  [`rectify()`](https://nacnudus.github.io/unpivotr/reference/rectify.md)
  give a more informative error message for non-distinct cells, for
  example when trying to pass cells from more than one sheet to these
  functions ([@gregrs-uk](https://github.com/gregrs-uk),
  [\#15](https://github.com/nacnudus/unpivotr/issues/15)).

## unpivotr 0.4.0

CRAN release: 2018-06-26

This version makes some big breaking changes for the sake of a more
intuitive grammar. It comes with much more documentation in the online
book [Spreadsheet Munging
Strategies](https://nacnudus.github.io/spreadsheet-munging-strategies/).

The main new workhorses:

- [`behead()`](https://nacnudus.github.io/unpivotr/reference/behead.md)
  takes one level of headers from a pivot table and make it part of the
  data. Chain this function to gradually strip every level of header
  away until you have tidy data.
- [`spatter()`](https://nacnudus.github.io/unpivotr/reference/spatter.md)
  is a data-type aware version of
  [`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html)
  and is a common final step.
- [`partition()`](https://nacnudus.github.io/unpivotr/reference/partition.md)
  breaks up small-multiples on a single sheet, so you can handle them
  individually.
- [`rectify()`](https://nacnudus.github.io/unpivotr/reference/rectify.md)
  visualises the cells in the console as they would look in a
  spreadsheet.

### Breaking changes

The previous version can be installed as follows.

``` r

devtools::install_version("unpivotr", version = "0.3.1", repos = "http://cran.us.r-project.org")
```

- The family of functions `NNW()` etc. has been removed in favour of the
  verbose `join_header()`, which has itself been renamed to
  [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  to suggest its similarity to
  [`behead()`](https://nacnudus.github.io/unpivotr/reference/behead.md)
  (though they are *not* complements).
- [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()` now follows the tidyverse convention of
  `fct` for ‘factor’ and `ord` for ‘ordered factor’.
- [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()`) now uses `col_names` and `row_names` as
  arguments instead of `colnames` and `rownames`, for consistency with
  tidyr.

### New features

- [`behead()`](https://nacnudus.github.io/unpivotr/reference/behead.md)
  is takes one level of headers from a pivot table and make it part of
  the data. Think of it like
  [`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html),
  except that it works when there is more than one row of headers (or
  more than one column of row-headers), and it only works on tables that
  have first come through
  [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()` or
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html).
- [`rectify()`](https://nacnudus.github.io/unpivotr/reference/rectify.md)
  displays cells as though in a spreadsheet, rather than in the ‘melted’
  form of
  [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()`) and
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html).
  This is useful for understanding the structure of a pivot table as a
  human, when planning how to unpivot it. A print method is available to
  render large datasets in the browser or the RStudio viewer pane.
- [`partition()`](https://nacnudus.github.io/unpivotr/reference/partition.md)
  divides a grid of cells into partitions containing individual tables.
  Give it the corner cells of each table on a spreadsheet.
- [`pack()`](https://nacnudus.github.io/unpivotr/reference/pack.md)
  packs cells values from separate columns per data type into one
  list-column.
  [`unpack()`](https://nacnudus.github.io/unpivotr/reference/pack.md) is
  the complement.
- [`isolate_sentinels()`](https://nacnudus.github.io/unpivotr/reference/isolate_sentinels.md)
  move sentinel values into a separate column, leaving `NA` behind (or
  `NULL` for list-columns).
- [`spatter()`](https://nacnudus.github.io/unpivotr/reference/spatter.md)
  is like
  [`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html),
  but preserves mixed data types.
- [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()`) now returns a `data_type` column that names
  the column that contains the value of a cell, similar to
  [`tidyxl::xlsx_cells()`](https://nacnudus.github.io/tidyxl/reference/xlsx_cells.html).
- [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()` now follows the tidyverse convention of
  `fct` for ‘factor’ and `ord` for ‘ordered factor’.
- [`enhead()`](https://nacnudus.github.io/unpivotr/reference/enhead.md)
  (formerly `join_header()`) gains a `drop = TRUE` argument to control
  whether to discard cells that don’t have a matching header (e.g. ones
  that are left of the leftmost header in `enhead(x, y, "NNW")`).
- [`justify()`](https://nacnudus.github.io/unpivotr/reference/justify.md)
  moves one set of cells to the same positions as another set. This is
  useful when header cells aren’t at the corner of the cells they
  describle. Put the header cells into
  [`justify()`](https://nacnudus.github.io/unpivotr/reference/justify.md),
  along with cells that *are* at the corner.
- New vignette ‘worked-examples’ of common tasks when munging
  spreadsheets.
- The ‘small-multiples’ vignette has been refactored to use the new
  features.
- `purpose` (built-in dataset) gains a new list-member
  `small-multiples`.

### Under the hood

- No longer depends on the data.table package.

### Many other tweaks especially to documentation

## unpivotr 0.3.1

CRAN release: 2018-01-20

- Performance improvements to
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md).
- Fixed a CRAN test on some platforms.

## unpivotr 0.3.0

CRAN release: 2017-12-03

- Made compatible with tidyxl version 1.0.0 (avoids
  [`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html),
  which doesn’t handle list columns).
- Updated to use the new `dplyr`/`rlang` combination instead of the old
  `dplyr`/`lazyeval` one.

## unpivotr 0.2.1

CRAN release: 2017-07-02

This release overhauls the
[`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
function of unpivotr to preserve the original data types of table cells
and to support HTML tables.

- `tidytable()` has been renamed
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md).
  `tidytable()` is an error, rather than a deprecation warning, because
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  is so different from before.
- There is a new
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  method and vignette for HTML.
- There is no
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  method for matrices. Convert matrices to data.frames first, choosing
  what to do with row and column names.
- [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  returns only relevant columns, according to the data types of the
  columns in the given data frame. It uses
  [`tibble::type_sum()`](https://pillar.r-lib.org/reference/type_sum.html)
  to determine the column type and to name the columns, so whereas
  characters used to be returned in a column called `character`, they
  are now returned in a column called `chr`. The full list of column
  names is in
  [`?tidy_table`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  and is `chr`, `cplx`, `cplx`, `dbl`, `fctr`, `int`, `lgl`, `list`. The
  columns `fctr` and `list` are list-columns, where each element is
  itself a list. This means that factors with different levels are kept
  separate. For HTML tables, an `html` column is returned containing the
  standalone HTML of each cell.
- Both `rowname` and `colname` arguments to
  [`tidy_table()`](https://nacnudus.github.io/unpivotr/reference/tidy_table.md)
  now default to `FALSE`.
- All functions consistently return a `tibble`.
- Some error messages are more helpful.

## unpivotr 0.1.1

CRAN release: 2017-04-23

- Fixed breakages introduced by dplyr 0.6.

## unpivotr 0.1.0

CRAN release: 2017-01-31

- Moved images from `inst/extdata` to `vignettes`.

## unpivotr 0.1.0

CRAN release: 2017-01-31

- Bumped version to match tidyxl, which is now more mature.
- Updated README and vignettes to use the new tidyxl api.

## unpivotr 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
