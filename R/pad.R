#' Pad a bag of cells with blanks
#'
#' @description When source data is sparse (e.g. spreadsheets from the 'tidyxl'
#' package), the output of \code{\link{tidy_table}} may also be sparse.  To fill
#' the gaps with blank cells, use \code{pad}.  Internally, \code{pad} is used to
#' ensure that functions like \code{\link{offset}} and \code{\link{extend}}
#' behave intuitively, as though blank cells do actually exist.
#'
#' @param cells Data frame, the cells to pad
#' @param rows Numeric vector, the rows to be padded (existing cells will never
#' be discarded, e.g. if this is 0, and no gaps will be left even if this is
#' outside the original range of rows)
#' @param cols Numeric vector, the columns to be padded (existing cells will
#' never be discarded, e.g. if this is 0, and no gaps will be left even if this
#' is outside the original range of columns)
#' @export
#' @examples
#' cells <-
#'   tidy_table(purpose$`NNW WNW`) %>%
#'   dplyr::filter(!is.na(chr)) # Introduce 'holes' in the data
#' # Select a region with gaps
#' (bag <- dplyr::filter(cells, row %in% 2:4, col %in% 1:2))
#' # Pad the gaps
#' pad(bag) # By default, the selection is squared-off to its width and height
#' pad(bag, 0, 0) # Zeros are equivalent to the defaults
#' pad(bag, 2:5, 1:3) # Add a row and a column
#' pad(bag, 6, 4) # No gaps are left even when .rows or .cols are distant
pad <- function(cells, rows = cells$row, cols = cells$col) {
  if (any(rows < 0) | any(cols < 0)) {
    stop("'rows' and 'cols' must be >= 0")
  }
  rows <- rows[rows != 0L]
  cols <- cols[cols != 0L]
  if (nrow(cells) == 0 & (length(rows) == 0 | length(cols) == 0)) {
    # Neither any cells provided, nor any rows/cols to pad
    return(cells)
  }
  # Pad potentials
  padding <- tidyr::crossing(row = tidyr::full_seq(c(cells$row, rows), 1L),
                         col = tidyr::full_seq(c(cells$col, cols), 1L))
  out <- dplyr::full_join(padding, cells, by = c("row", "col"))
  tibble::as_tibble(out)
}

