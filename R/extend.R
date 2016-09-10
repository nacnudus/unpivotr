#' Extend a bag of cells in one direction, optionally up to a boundary
#' condition.
#'
#' @description A bag of data cells is a data frame with at least the
#' columns 'row' and 'col', as well as any others that carry information about
#' the cells, e.g. their values.  More cells may be added to the bag by
#' extending the bag in a given direction.  Unless a boundary condition is
#' specified, all cells up to the edge of the sheet will be added.
#' @param bag Data frame. The original selection, including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param cells Data frame. All the cells in the sheet, within which to extend
#' the bag. Including at least the columns 'row' and 'column', as well as any
#' columns referred to by the bounadry formula.
#' @param direction Character vector length 1. The direction in which to extend,
#' among the compass directions "N", "E", "S", "W", where "N" is north (up).
#' @param boundary Formula to express a boundary condition. Defaults to
#' `FALSE`, which means the extension will go to the boundary of the sheet. `~
#' col <= 50` would up to the 50th column, but see 'details' for how this might
#' behave unpredictably depending on 'include'.
#' @param include Logical vector length 1. Whether to include in the extension
#' the first cell at which the boundary condition is met.  Can be unpredictable
#' when `TRUE` if `boundary` is something like `~ col <= 50`, because if there
#' is no cell in the 50th column, then the first cell beyond the 50th column
#' will be included.
#' @details A bag may have ragged rows or ragged cols. Gaps will not be filled
#' in.
#' @export
#' @examples
#' # Please see the vignette for examples (not yet written in vignette, sorry!)
#' vignette("compass-directions", "unpivotr")
extend <- function(bag, cells, direction, boundary = FALSE, include = FALSE) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  if (direction == "N") {
    extend_N(bag, cells, boundary, include)
  } else if (direction == "E") {
    extend_E(bag, cells, boundary, include)
  } else if (direction == "S") {
    extend_S(bag, cells, boundary, include)
  } else if (direction == "W") {
    extend_W(bag, cells, boundary, include)
  }
}

extend_N <- function(bag, cells, boundary, include) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  bag %>%
    dplyr::group_by(col) %>%
    dplyr::do({
      bagrow <- .
      cells %>%
        # Look in the relevant row
        dplyr::filter(col == bagrow$col[1], row < min(bagrow$row)) %>%
        dplyr::arrange(-row) %>%
        dplyr::mutate_(boundary = boundary) %>% # Apply the rule
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

extend_E <- function(bag, cells, boundary, include) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  bag %>%
    dplyr::group_by(row) %>%
    dplyr::do({
      bagrow <- .
      cells %>%
        # Look in the relevant row
        dplyr::filter(row == bagrow$row[1], col > max(bagrow$col)) %>%
        dplyr::arrange(col) %>%
        dplyr::mutate_(boundary = boundary) %>% # Apply the rule
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

extend_S <- function(bag, cells, boundary, include) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  bag %>%
    dplyr::group_by(col) %>%
    dplyr::do({
      bagrow <- .
      cells %>%
        # Look in the relevant row
        dplyr::filter(col == bagrow$col[1], row > max(bagrow$row)) %>%
        dplyr::arrange(row) %>%
        dplyr::mutate_(boundary = boundary) %>% # Apply the rule
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

extend_W <- function(bag, cells, boundary, include) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  bag %>%
    dplyr::group_by(row) %>%
    dplyr::do({
      bagrow <- .
      cells %>%
        # Look in the relevant row
        dplyr::filter(row == bagrow$row[1], col < min(bagrow$col)) %>%
        dplyr::arrange(-col) %>%
        dplyr::mutate_(boundary = boundary) %>% # Apply the rule
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

# TODO: Extend by a number of rows/cells, not only up to a boundary
# TODO: Extend to the first blank cell
