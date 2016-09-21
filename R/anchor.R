#' Filter for a cell from which to select others
#'
#' @description \code{anchor} protects you from selecting an initial cell that
#' does not exist.  This is useful when choosing an inital cell from which to
#' search for others, using \code{\link{offset}} and \code{\link{extend}}.
#' If the chosen cell does not exist, then cell is generated at the given
#' row and column, with NA in every column other than 'row' and 'col'.
#' @param cells Data frame, the cells among which the anchor will be sought
#' @param row Numeric, the row of the intended cell
#' @param col Numeric, the column of the intended cell
#' @export
#' @examples
#' cells <- tidytable(purpose$`NNW WNW`)
#' \dontrun{
#'   anchor(cells, 0, 1)
#' }
#' anchor(cells, 1, 1)
#' anchor(cells, 100, 1)
anchor <- function(cells, row, col) {
  if (row <= 0 || col <= 0) {stop("'row' and 'col' must both be >= 1")}
  out <- cells[cells$row == row & cells$col == col, ]
  if (nrow(out) == 0) {
    out <- dplyr::bind_rows(out, tibble::tibble(row = row, col = col))
  }
  out
}
