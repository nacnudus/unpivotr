#' Offset a bag of cells by some rows or columns.
#'
#' @description A bag of data cells is a data frame with at least the
#' columns 'row' and 'col', as well as any others that carry information about
#' the cells, e.g. their values.  The position of this bag may be moved across
#' the sheet, exchanging the cells which are included in the bag.  Non-existant
#' cells will be padded, so chains of offets preserve the shape of the original
#' bag.
#' @param bag Data frame. The original selection, including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param cells Data frame. All the cells in the sheet, within which to extend
#' the bag. Including at least the columns 'row' and 'column', as well as any
#' columns referred to by the bounadry formula.
#' @param rows Numeric vector length 1. The number of rows by which to offset.
#' Negative values offset upwards.
#' @param cols Numeric vector length 1. The number of columns by which to
#' offset.  Negative values offset left.
#' @export
#' @examples
#' cells <- 
#'   tidytable(purpose$`NNW WNW`, FALSE, FALSE) %>% 
#'   dplyr::filter(!is.na(character)) # Introduce 'holes' in the data
#' # Select an L-shape with gaps
#' bag <- dplyr::filter(cells, row %in% 3:4, col %in% 1:2)
#' # Offset and pad (actually anchor) the gaps (now in different parts of the L)
#' offset(bag, cells, -1, 1)
offset <- function(bag, cells, rows = 0, cols = 0) {
  bag %>%
    dplyr::select(row, col) %>%
    dplyr::mutate(row = row + rows, col = col + cols) %>%
    dplyr::rowwise() %>%
    dplyr::do(anchor(cells, .$row, .$col))
}


