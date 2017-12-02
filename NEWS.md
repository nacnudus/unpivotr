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



