# unpivotr (development version)

* Change `"NNW"` to `"up-left"` etc.  Compass directions still work, but the
    built-in dataset `purpose` has different names and documentation will
    gradually change to the new terms.
* Patch for upcoming upstream changes

# unpivotr 0.5.1

* Patch for tibble `.name_repair` compatibility (#2144 @krlmlr)
* Patch to switch to `tidyselect` from `dplyr` functions.

# unpivotr 0.5.0

## New features

* `behead_if()` is for tiered headers within the same row or column.  It takes
    filter functions similarly to `dplyr::filter()` to decide which cells to
    treat as headers, and can be applied more than once to the same row or
    column of headers until every tier has been dealt with.
* `merge_rows()` and `merge_cols()` combines header text when it is split over
    multiple cells.

## Other changes

* `behead()`, `enhead()`, `partition()` and `rectify()` give a more informative
    error message for non-distinct cells, for example when trying to pass cells
    from more than one sheet to these functions (@gregrs-uk, #15).

# unpivotr 0.4.0

This version makes some big breaking changes for the sake of a more intuitive
grammar.  It comes with much more documentation in the online book [Spreadsheet
Munging Strategies](https://nacnudus.github.io/spreadsheet-munging-strategies/).

The main new workhorses:

* `behead()` takes one level of headers from a pivot table and make it part of
    the data.  Chain this function to gradually strip every level of header away
    until you have tidy data.
* `spatter()` is a data-type aware version of `tidyr::spread()` and is a
    common final step.
* `partition()` breaks up small-multiples on a single sheet, so you can handle
    them individually.
* `rectify()` visualises the cells in the console as they would look in a
    spreadsheet.

## Breaking changes

The previous version can be installed as follows.

```r
devtools::install_version("unpivotr", version = "0.3.1", repos = "http://cran.us.r-project.org")
```

* The family of functions `NNW()` etc. has been removed in favour of the verbose
    `join_header()`, which has itself been renamed to `enhead()` to suggest its
    similarity to `behead()` (though they are *not* complements).
* `enhead()` (formerly `join_header()` now follows the tidyverse convention of
    `fct` for 'factor' and `ord` for 'ordered factor'.
* `enhead()` (formerly `join_header()`) now uses `col_names` and `row_names` as
    arguments instead of `colnames` and `rownames`, for consistency with tidyr.

## New features

* `behead()` is takes one level of headers from a pivot table and make it part
    of the data.  Think of it like `tidyr::gather()`, except that it works when
    there is more than one row of headers (or more than one column of
    row-headers), and it only works on tables that have first come through
    `enhead()` (formerly `join_header()` or `tidyxl::xlsx_cells()`.
* `rectify()` displays cells as though in a spreadsheet, rather than in the
    'melted' form of `enhead()` (formerly `join_header()`) and
    `tidyxl::xlsx_cells()`.  This is useful for understanding the structure of a
    pivot table as a human, when planning how to unpivot it.  A print method is
    available to render large datasets in the browser or the RStudio viewer
    pane.
* `partition()` divides a grid of cells into partitions containing individual
    tables.  Give it the corner cells of each table on a spreadsheet.
* `pack()` packs cells values from separate columns per data type into one
    list-column.  `unpack()` is the complement.
* `isolate_sentinels()` move sentinel values into a separate column, leaving
    `NA` behind (or `NULL` for list-columns).
* `spatter()` is like `tidyr::spread()`, but preserves mixed data types.
* `enhead()` (formerly `join_header()`) now returns a `data_type` column that
    names the column that contains the value of a cell, similar to
    `tidyxl::xlsx_cells()`.
* `enhead()` (formerly `join_header()` now follows the tidyverse convention of
  `fct` for 'factor' and `ord` for 'ordered factor'.
* `enhead()` (formerly `join_header()`) gains a `drop = TRUE` argument to
  control whether to discard cells that don't have a matching header (e.g. ones
  that are left of the leftmost header in `enhead(x, y, "NNW")`).
* `justify()` moves one set of cells to the same positions as another set.  This
    is useful when header cells aren't at the corner of the cells they describle.
    Put the header cells into `justify()`, along with cells that *are* at the
    corner.
* New vignette 'worked-examples' of common tasks when munging spreadsheets.
* The 'small-multiples' vignette has been refactored to use the new features.
* `purpose` (built-in dataset) gains a new list-member `small-multiples`.

## Under the hood

* No longer depends on the data.table package.

## Many other tweaks especially to documentation

# unpivotr 0.3.1

* Performance improvements to `tidy_table()`.
* Fixed a CRAN test on some platforms.

# unpivotr 0.3.0

* Made compatible with tidyxl version 1.0.0 (avoids `dplyr::distinct()`, which
    doesn't handle list columns).
* Updated to use the new `dplyr`/`rlang` combination instead of the old
    `dplyr`/`lazyeval` one.

# unpivotr 0.2.1

This release overhauls the `tidy_table()` function of unpivotr to preserve the
original data types of table cells and to support HTML tables.

* `tidytable()` has been renamed `tidy_table()`.  `tidytable()` is an error,
    rather than a deprecation warning, because `tidy_table()` is so different
    from before.
* There is a new `tidy_table()` method and vignette for HTML.
* There is no `tidy_table()` method for matrices.  Convert matrices to
    data.frames first, choosing what to do with row and column names.
* `tidy_table()` returns only relevant columns, according to the data types of
    the columns in the given data frame.  It uses `tibble::type_sum()` to
    determine the column type and to name the columns, so whereas characters
    used to be returned in a column called `character`, they are now returned in
    a column called `chr`.  The full list of column names is in `?tidy_table`
    and is `chr`, `cplx`, `cplx`, `dbl`, `fctr`, `int`, `lgl`, `list`.  The
    columns `fctr` and `list` are list-columns, where each element is itself a
    list.  This means that factors with different levels are kept separate.  For
    HTML tables, an `html` column is returned containing the standalone HTML of
    each cell.
* Both `rowname` and `colname` arguments to `tidy_table()` now default to
    `FALSE`.
* All functions consistently return a `tibble`.
* Some error messages are more helpful.

# unpivotr 0.1.1

* Fixed breakages introduced by dplyr 0.6.

# unpivotr 0.1.0

* Moved images from `inst/extdata` to `vignettes`.


# unpivotr 0.1.0

* Bumped version to match tidyxl, which is now more mature.
* Updated README and vignettes to use the new tidyxl api.

# unpivotr 0.0.0.9000

