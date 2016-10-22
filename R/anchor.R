#' Filter for a cell from which to select others
#'
#' @description \code{anchor} protects you from selecting initial cells that do
#' not exist.  This is useful when choosing inital cells from which to search
#' for others, using \code{\link{offset}} and \code{\link{extend}}.  If the
#' chosen cells do not exist, then cells are generated at the given rows and
#' columns, with NAs in every column other than 'row' and 'col'.
#' @param cells Data frame, the cells among which the anchor will be sought
#' @param rows Numeric, the rows of the intended cells
#' @param cols Numeric, the columns of the intended cell
#' @param cross Logical, whether to 'cross' the given rows and columns to
#' generate all combinations. Unless this is TRUE, \code{rows} and \code{cols}
#' must be the same length.  TRUE by default.
#' @export
#' @examples
#' cells <- tidytable(purpose$`NNW WNW`)
#' \dontrun{
#'   anchor(cells, 0, 1)
#'   anchor(cells, 1, 1:2)
#' }
#' anchor(cells, 1, 1)
#' anchor(cells, 1, 1:2, cross = TRUE)
#' anchor(cells, 100, 1)
anchor <- function (cells, rows, cols, cross = TRUE) {
    if (any(c(rows, cols) <= 0)) {
        stop("Elements of 'rows' and 'cols' must all be >= 1")
    }
    rows <- as.integer(rows)
    cols <- as.integer(cols)
    if (cross) {
      left <- tidyr::crossing(row = rows, col = cols)
    } else {
      left <- tibble::data_frame(row = rows, col = cols)
    }
    dplyr::left_join(left, cells, by = c("row", "col"))
}
