#' Un-pivot complex and irregular data layouts.
#'
#' 'Unpivotr' provides tools for converting data from complex or irregular
#' layouts to a columnar structure.  For example, tables with multi-level column
#' or row headers, or spreadsheets of several tables, nested HTML tables, and
#' data that uses several different sentinel values.
#'
#' The best way to learn unpivotr is the free online book [Spreadsheet Munging
#'  Strategies](https://nacnudus.github.io/spreadsheet-munging-strategies/).
#'
#' Header and data cells can selected by their contents, position, data type and
#' formatting, and can be associated with one other by their relative positions.
#'
#' The input data must be a data frame with the columns 'row' and 'col' to
#' describe the position of a 'cell' of data.  For cells that are to be
#' interpreted as data, further columns containing the 'value' of the cell are,
#' of course, necessary for there to be any point in using this package, though
#' they are not actually required for any of the given functions.
#'
#' Data frames and HTML tables can be converted into a format meeting these
#' requirements by using the [as_cells()] function.  Excel (.xlsx)
#' files can be imported directly into the required format with the 'tidyxl'
#' package, available at <https://github.com/nacnudus/tidyxl>, which has the
#' advantage that it retains cell formatting and comments.
#'
"_PACKAGE"
# > [1] "_PACKAGE"
