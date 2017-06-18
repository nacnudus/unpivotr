#' Un-pivot complex and irregular data layouts.
#'
#' 'Unpivotr' provides tools for converting data from complex or irregular
#' layouts to a columnar structure.  For example, tables with multi-level column
#' or row headers, or spreadsheets, or nested HTML tables.  Header and data
#' cells can selected by their contents, position and formatting, and can be
#' associated with one other by their relative positions.
#'
#' The input data must be a data frame with the columns 'row' and 'col' to
#' describe the position of a 'cell' of data.  For cells that are to be
#' interpreted as data, further columns containing the 'value' of the cell are,
#' of course, necessary for there to be any point in using this package, though
#' they are not actually required for any of the given functions.
#'
#' Data frames and HTML tables can be converted into a format meeting these
#' requirements by using the \code{\link{tidy_table}} function.  Excel (.xlsx)
#' files can be imported directly into the required format with the 'tidyxl'
#' package, available at \url{https://github.com/nacnudus/tidyxl}, which has the
#' advantage that it retains cell formatting and comments.
#'
#' The functions are documented in \code{\link{join_header}},
#' \code{\link{extend}} and \code{\link{offset}}.  All functions are designed to
#' work well with the pipe \code{\%>\%} from the magrittr package.
#'
"_PACKAGE"
#> [1] "_PACKAGE"

