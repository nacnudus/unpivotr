#' Merge cell values into a single cell by rows or columns
#'
#' @description When a single column header is split across cells, merge the
#' cells with [merge_rows()] or [merge_cols()].  E.g. if a column header "Mean
#' GDP" is split over two cells, where the top cell has the value "Mean" and the
#' bottom cell has the value "GDP", then [merge_rows()] will combine them into a
#' single cell with the value "Mean GDP".
#'
#' [merge_rows()] keeps the top cell, and [merge_cols()] keeps the left-most
#' cell.  When there are several columns of headers, [merge_rows()] aligns the
#' output cells so that they are all in the same row, and similarly
#' [merge_cols()] aligns to the same column.
#'
#' These functions apply only to cells with character values because it doesn't
#' make sense to concatenate non-character values.  Convert cell values to
#' characters first if you need to merge non-character cells.
#'
#' @param cells Data frame. The cells of a pivot table, usually the output of
#'   [as_cells()] or [tidyxl::xlsx_cells()], or of a subsequent operation on
#'   those outputs.
#' @param rows The numbers of the rows to be merged.
#' @param cols The numbers of the columns to be merged.
#' @param values The column of `cells` to use as the values of each cell to be
#'   merged.  Given as a bare variable name.
#' @param collapse A character string to separate the values of each cell.
#'
#' @return A data frame
#'
#' @name merge_cells
#' @examples
#'  x <- tibble::tribble(
#' ~row, ~col, ~data_type,     ~chr,
#'    1,    1,      "chr",   "Katy",
#'    2,    1,      "chr",  "Perry",
#'    3,    1,      "chr",      "a",
#'    4,    1,      "chr",      "b",
#'    5,    1,      "chr",      "c",
#'    2,    2,      "chr",  "Adele",
#'    3,    2,      "chr",      "d",
#'    4,    2,      "chr",      "e",
#'    5,    2,      "chr",      "f",
#'    1,    3,      "chr", "Ariana",
#'    2,    3,      "chr", "Grande",
#'    3,    3,      "chr",      "g",
#'    4,    3,      "chr",      "h",
#'    5,    3,      "chr",      "i"
#' )
#' rectify(x)
#' y <- merge_rows(x, 1:2, chr)
#' rectify(y)
#' z <- merge_cols(x, 1:2, chr)
#' rectify(z)
NULL

#' @rdname merge_cells
#' @export
merge_rows <- function(cells, rows, values, collapse = " ") {
  UseMethod("merge_rows")
}

#' @export
merge_rows.data.frame <- function(cells, rows, values, collapse = " ") {
  # Prepare to take the first value of all other columns in the summarise() step
  values <- rlang::ensym(values)
  non_values <- setdiff(
    names(cells),
    c(rlang::expr_text(rlang::enexpr(values)), "col")
  )
  summaries <- purrr::map(
    paste0("dplyr::first(", non_values, ")"),
    rlang::parse_expr
  )
  names(summaries) <- non_values
  # Merge the rows
  merged_cells <-
    dplyr::filter(cells, row %in% rows) %>%
    dplyr::mutate(row = min(row)) %>%
    dplyr::arrange(col, row) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(
      !!!summaries,
      !!values := paste(!!values, collapse = collapse)
    )
  # Prepend to the rest of the cells and return
  other_cells <-
    cells %>%
    dplyr::filter(!(row %in% rows))
  dplyr::bind_rows(merged_cells, other_cells)
}

#' @rdname merge_cells
#' @export
merge_cols <- function(cells, cols, values, collapse = " ") {
  UseMethod("merge_cols")
}

#' @export
merge_cols.data.frame <- function(cells, cols, values, collapse = " ") {
  # Prepare to take the first value of all other columns in the summarise() step
  values <- rlang::ensym(values)
  non_values <- setdiff(
    names(cells),
    c(rlang::expr_text(rlang::enexpr(values)), "row")
  )
  summaries <- purrr::map(
    paste0("dplyr::first(", non_values, ")"),
    rlang::parse_expr
  )
  names(summaries) <- non_values
  # Merge the rows
  merged_cells <-
    dplyr::filter(cells, col %in% cols) %>%
    dplyr::mutate(col = min(col)) %>%
    dplyr::arrange(row, col) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(
      !!!summaries,
      !!values := paste(!!values, collapse = collapse)
    )
  # Prepend to the rest of the cells and return
  other_cells <-
    cells %>%
    dplyr::filter(!(col %in% cols))
  dplyr::bind_rows(merged_cells, other_cells)
}
