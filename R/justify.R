#' Align one set of cells with another set
#'
#' @description
#' If the header cells of a table aren't aligned to the left, right, top or
#' bottom of the data cells that they describe, then use [justify()] to re-align
#' them, using a second set of cells as a guide.
#'
#' @param header_cells Data frame of data cells with at least the columns 'row'
#'   and 'column', which are `numeric` or `integer`.
#' @param corner_cells Data frame of header cells with at least the columns
#'   'row' and 'column', which are numeric/integer vectors.  The same length as
#'   `header_cells`.
#'
#' @name justify
#' @export
#' @examples
#' header_cells <- tibble::tibble(row = c(1L, 1L, 1L, 1L),
#'                                col = c(3L, 5L, 8L, 10L),
#'                                value = LETTERS[1:4])
#' corner_cells <- tibble::tibble(row = c(2L, 2L, 2L, 2L),
#'                                col = c(1L, 4L, 6L, 9L))
#' justify(header_cells, corner_cells)
justify <- function(header_cells, corner_cells) {
  UseMethod("justify")
}

#' @export
justify.data.frame <- function(header_cells, corner_cells) {
  stopifnot(nrow(header_cells) == nrow(corner_cells))
  header_cells <- dplyr::arrange(header_cells, row, col)
  corner_cells <- dplyr::arrange(corner_cells, row, col)
  header_cells$row <- corner_cells$row
  header_cells$col <- corner_cells$col
  header_cells
}
