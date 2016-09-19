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
#' @param boundary Formula to express a boundary condition, or "blank". Defaults
#' to `FALSE`, which means the extension will go to the boundary of the sheet.
#' `~ col <= 50` would go up to the 50th column. "blank" goes up to just before
#' a blank row/col when 'include' is FALSE, otherwise it goes up to the next
#' non-blank cell after a blank one.
#' @param include Logical vector length 1. Whether to include in the extension
#' the first cell at which the boundary condition is met.  Can be unpredictable
#' when `TRUE` if `boundary` is something like `~ col <= 50`, because if there
#' is no cell in the 50th column, then the first cell beyond the 50th column
#' will be included.
#' @details A bag may have ragged rows or ragged cols. Gaps will not be filled
#' in.
#' @name extend
#' @export
#' @examples
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- tidytable(x, rownames = FALSE, colnames = FALSE)
#' cells <- cells[!is.na(cells$character), ]
#' # Select a particular cell
#' cell <- cells[which(cells$row == 10 & cells$col == 3), ]
#' # Extend the selection upwards, stopping before the NA.
#' extend_N(cell, cells, boundary = "blank")
#' # Extend the selection right, up to and including the fifth column.
#' extend_E(cell, cells, boundary = ~ col == 5, include = TRUE)
extend <- function(bag, cells, direction, boundary = FALSE, include = FALSE) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  if (direction %in% c("N", "E", "S", "W")) {
    do.call(paste0("extend_", direction), list(bag, cells, boundary, include))
  } else {
    stop("'direction' must be one of 'N', 'E', 'S', 'W'")
  }
}

#' @describeIn extend Extend a bag of cells to the north
#' @export
extend_N <- function(bag, cells, boundary = FALSE, include = FALSE) {
  # Extends an existing bag of cells along an axis up to a boundary, by row or
  # by column depending on the axis.
  # Bag may be ragged rows or ragged cols, but gaps will not be filled in.
  if (boundary == "blank") {
    boundary = ~ dplyr::lag(row, default = min(bag$row)) - row > 1
  }
  bag %>%
    dplyr::group_by(col) %>%
    dplyr::do({
      bagrow <- .
        cells %>%
        # Look in the relevant row
        dplyr::filter(col == bagrow$col[1], row < min(bagrow$row)) %>%
        dplyr::arrange(-row) %>%
        dplyr::mutate_(boundary = boundary) %>% # Apply the rule
        tidyr::replace_na(list(boundary = 0)) %>%
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)

    }) %>%
    dplyr::ungroup()
}

#' @describeIn extend Extend a bag of cells to the east
#' @export
extend_E <- function(bag, cells, boundary = FALSE, include = FALSE) {
  if (boundary == "blank") {
    boundary = ~ col - dplyr::lag(col, default = max(bag$col)) > 1
  }
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
        tidyr::replace_na(list(boundary = 0)) %>%
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

#' @describeIn extend Extend a bag of cells to the south
#' @export
extend_S <- function(bag, cells, boundary = FALSE, include = FALSE) {
  if (boundary == "blank") {
    boundary = ~ row - dplyr::lag(row, default = max(bag$row)) > 1
  }
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
        tidyr::replace_na(list(boundary = 0)) %>%
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

#' @describeIn extend Extend a bag of cells to the west
#' @export
extend_W <- function(bag, cells, boundary = FALSE, include = FALSE) {
  if (boundary == "blank") {
    boundary = ~ dplyr::lag(col, default = min(bag$col)) - col > 1
  }
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
        tidyr::replace_na(list(boundary = 0)) %>%
        # Take cells up to (and conditionally including) boundary
        dplyr::filter(cumsum(cumsum(boundary)) <= include) %>%
        dplyr::select(-boundary) %>%
        dplyr::bind_rows(bagrow)
    }) %>%
    dplyr::ungroup()
}

# TODO: Extend by a number of rows/cells, not only up to a boundary
# TODO: Extend to the first blank cell
