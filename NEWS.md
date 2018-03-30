# unpivotr 0.3.1.9000

## Breaking changes

* `tidy_table()` now follows the tidyverse convention of `fct` for 'factor' and
    `ord` for 'ordered factor'.

## New features

* `behead()` is takes one level of headers from a pivot table and make it part
    of the data.  Think of it like `tidyr::gather()`, except that it works when
    there is more than one row of headers (or more than one column of
    row-headers), and it only works on tables that have first come through
    `tidy_table()` or `tidyxl::xlsx_cells()`.
* `rectify()` displays cells as though in a spreadsheet, rather than in the
    'melted' form of `tidy_table()` and `tidyxl::xlsx_cells()`.  This is useful
    for understanding the structure of a pivot table as a human, when planning
    how to unpivot it.  A print method is available to render large datasets in
    the browser or the RStudio viewer pane.
* `partition()` divides a grid of cells into partitions containing individual
    tables.  Give it the corner cells of each table on a spreadsheet, and it
    returns all the cells of the spreadsheet with a `partition` column that
    identifies which cells belong to which table.
* `pack()` packs cells values from separate columns per data type into one
    list-column.  `unpack()` is the complement.
* `isolate_sentinels()` move sentinel values into a separate column, leaving
    `NA` behind (or `NULL` for list-columns).
* `spatter()` is like `tidyr::spread()`, but preserves mixed data types.
* `tidy_table()` now returns a `data_type` column that names the column that
    contains the value of a cell, similar to `tidyxl::xlsx_cells()`.
* `tidy_table()` now follows the tidyverse convention of `fct` for 'factor' and
    `ord` for 'ordered factor'.
* `join_header()` and the related functions `N()`, `NNW()`, etc. gains a `drop =
    TRUE` argument to control whether to discard cells that don't have a
    matching header (e.g. ones that are left of the leftmost header in `NNW()`).
* `join_header()` encourages names `N`, `NNW` rather than quoted strings `"N"`,
    `"NNW"`, etc. in line with the tidyverse standard, because it names an
    actual object (a function).
* New vignette 'worked-examples' of common tasks when munging spreadsheets.
* `purpose` (built-in dataset) gains a new list-member `small-multiples`.

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

* Added a `NEWS.md` file to track changes to the package.



