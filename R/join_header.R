#' Join a bag of data cells to some header, by proximity in a given direction,
#' e.g. NNW searches up and up-left from a data cell to find a header cell.
#'
#' @description A bag of data cells is a data frame with at least the
#' columns 'row' and 'col', as well as any others that carry information about
#' the cells, e.g. their values.  Cells in a table are associated with header
#' cells by proximity.  Having collected header cells and data cells into
#' separate data frames, 'join_header' and the related functions 'NNW', 'ABOVE',
#' etc., join the values in the header cells to the data cells, choose the
#' nearest header to each cell, in a given direction.
#' @param bag Data frame. A bag of data cells including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param header Data frame. A bag of header cells, which currently must have
#' the columns 'row', 'col' and 'header', where 'header' is a column whose value
#' describes the data.  For example, for data cells recording a number of
#' animals, and a header describing the type of animal, then the 'header' column
#' might contain the values `c("dog", "cat", "mouse")`.
#' @param boundary Data frame. Only applies to the directions "ABOVE", "RIGHT",
#' "BELOW" and "LEFT".  A bag of cells in one row or one column,
#' demarking boundaries within which to match headers with cells.  For example,
#' a boundary could be a bag of cells with borders on one side.  This is useful
#' when the nearest header might be the wrong header because it lies on the 
#' other side of a border.
#' @param direction Character vector length 1. A compass direction to search for the nearest header.  See
#' 'details'.
#' @param colname Character vector length one. Column name to give the header
#' values once they are joined to the 'bag'.  Continuing the example, this might
#' be "animal_type".
#' @details Headers are associated with data by proximity in a given direction.
#' The directions are mapped to the points of the compass, where 'N' is north
#' (up), 'E' is east (right), and so on.  `join_header` finds the nearest header
#' to a given data cell in a given direction, and joins its value to the data
#' cell.  The most common directions to search are 'NNW' (for left-aligned
#' headers at the top of the table) and 'WNW' for top-aligned headers at the
#' side of the table.  There can be a tie in the directions 'ABOVE', 'BELOW',
#' 'LEFT' and 'RIGHT' (for headers that are not aligned to the edge of the data
#' cells that they refer to), and currently ties will cause the affected cells
#' not to be returned at all.  The full list of available directions is 'N',
#' 'E', 'S', 'W', 'NNW', 'NNE', 'ENE', 'ESE', 'SSE',
#' 'SSW', 'WSW', 'WNW'.  For convenience, these directions are provided as their
#' own functions, wrapping the concept of 'join_header'.
#' @name join_header
#' @export
#' @examples
#' library(dplyr)
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- tidytable(x, FALSE, FALSE)
#' cells <- cells[!is.na(cells$character), ]
#' head(cells)
#' # Select the cells containing the values
#' datacells <- 
#'   cells %>%
#'   filter(row == 3, col == 3) %>%
#'   extend_E(cells) %>%
#'   extend_S(cells)
#' head(datacells)
#' # Select the row headers
#' row_headers <- 
#'   cells %>%
#'   filter(col <= 2) %>%
#'   select(row, col, value = character) %>%
#'   split(.$col) # Separate each column of headers
#' row_headers
#' # Select the column headers
#' col_headers <- 
#'   cells %>%
#'   filter(row <= 2) %>%
#'   select(row, col, value = character) %>%
#'   split(.$row) # Separate each row of headers
#' col_headers
#' # From each data cell, search for the nearest one of each of the headers
#' datacells %>%
#'   NNW(col_headers$`1`, "sex") %>%
#'   N(col_headers$`2`, "purpose") %>%
#'   WNW(row_headers$`1`, "education") %>%
#'   W(row_headers$`2`, "age")
join_header <- function(bag, header, direction, colname, boundaries = NULL) {
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(bag, header, colname, boundaries))
  } else if (direction %in% c("N", "E", "S", "W", 
                             "NNW", "NNE", 
                             "ENE", "ESE", 
                             "SSE", "SSW", 
                             "WSW", "WNW")) {
    do.call(direction, list(bag, header, colname))
  } else {
    stop("The direction ", direction, 
         ", is either not recognised or not yet supported.")
  }
}

#' @describeIn join_header Join nearest header in the 'N' direction.
#' @export
N <- function(bag, header, colname) {
  domains <- N_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

N_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row) %>%
    # x1 and y1 are the cell itself, x2 is the same column as the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y1 = as.numeric(row),
           x2 = as.numeric(col)) %>%
    # y2 goes up to just before the next header in any column
    dplyr::group_by(row) %>%
    tidyr::nest() %>%
    dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'E' direction.
#' @export
E <- function(bag, header, colname) {
  domains <- E_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

E_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col) %>%
    # x1 and y1 are the cell itself, y2 is the same row as the cell itself
    dplyr::mutate(x2 = as.numeric(col),
           y2 = as.numeric(row),
           y1 = as.numeric(row)) %>%
    # x1 goes back to just after the previous header in any row
    dplyr::group_by(col) %>%
    tidyr::nest() %>%
    dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'S' direction.
#' @export
S <- function(bag, header, colname) {
  domains <- S_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

S_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row) %>%
    # x1 and y2 are the cell itself, x2 is the same column as the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y2 = as.numeric(row),
           x2 = as.numeric(col)) %>%
    # y1 goes back to just after the previous header in any column
    dplyr::group_by(row) %>%
    tidyr::nest() %>%
    dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'W' direction.
#' @export
W <- function(bag, header, colname) {
  domains <- W_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

W_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col) %>%
    # x1 and y1 are the cell itself, y2 is the same row as the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y1 = as.numeric(row),
           y2 = as.numeric(row)) %>%
    # x2 goes up to just before the next header in any row
    dplyr::group_by(col) %>%
    tidyr::nest() %>%
    dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'NNW' direction.
#' @export
NNW <- function(bag, header, colname) {
  domains <- NNW_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

NNW_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row, col) %>%
    # x1 and y1 are the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y1 = as.numeric(row)) %>%
    # x2 goes up to just before the next header in the row
    dplyr::group_by(row) %>%
    dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
    # y2 goes up to just before the next header in any column
    tidyr::nest() %>%
    dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'NNE' direction.
#' @export
NNE <- function(bag, header, colname) {
  domains <- NNE_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

NNE_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row, col) %>%
    # x2 and y1 are the cell itself
    dplyr::mutate(x2 = as.numeric(col),
           y1 = as.numeric(row)) %>%
    # x1 goes back to just after the previous header in the row
    dplyr::group_by(row) %>%
    dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
    # y2 goes up to just before the next header in any column
    tidyr::nest() %>%
    dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'ENE' direction.
#' @export
ENE <- function(bag, header, colname) {
  domains <- ENE_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

ENE_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col, row) %>%
    # x2 and y1 are the cell itself
    dplyr::mutate(x2 = as.numeric(col),
           y1 = as.numeric(row)) %>%
    # y2 goes up to just before the next header in the column
    dplyr::group_by(col) %>%
    dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
    # x1 goes back to just after the previous header in any row
    tidyr::nest() %>%
    dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'ESE' direction.
#' @export
ESE <- function(bag, header, colname) {
  domains <- ESE_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

ESE_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col, row) %>%
    # x2 and y2 are the cell itself
    dplyr::mutate(x2 = as.numeric(col),
           y2 = as.numeric(row)) %>%
    # y1 goes back to just after the previous header in the column
    dplyr::group_by(col) %>%
    dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
    # x1 goes back to just after the previous header in any row
    tidyr::nest() %>%
    dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'SSE' direction.
#' @export
SSE <- function(bag, header, colname) {
  domains <- SSE_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

SSE_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row, col) %>%
    # x2 and y2 are the cell itself
    dplyr::mutate(x2 = as.numeric(col),
           y2 = as.numeric(row)) %>%
    # x1 goes back to just after the previous header in any row
    dplyr::group_by(row) %>%
    dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
    # y1 goes back to just after the previous header in the column
    tidyr::nest() %>%
    dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'SSW' direction.
#' @export
SSW <- function(bag, header, colname) {
  domains <- SSW_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

SSW_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(row, col) %>%
    # x1 and y2 are the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y2 = as.numeric(row)) %>%
    # x2 goes up to just before the next header in any row
    dplyr::group_by(row) %>%
    dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
    # y1 goes back to just after the previous header in the column
    tidyr::nest() %>%
    dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'WSW' direction.
#' @export
WSW <- function(bag, header, colname) {
  domains <- WSW_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

WSW_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col, row) %>%
    # x1 and y2 are the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y2 = as.numeric(row)) %>%
    # y1 goes back to just after the previous header in the column
    dplyr::group_by(col) %>%
    dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
    # x2 goes up to just before the next header in any row
    tidyr::nest() %>%
    dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'WNW' direction.
#' @export
WNW <- function(bag, header, colname) {
  domains <- WNW_domains(header)
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

WNW_domains <- function(header) {
  # Join header to cells by proximity
  # First, the domain of each header
  domains <- 
    header %>%
    dplyr::arrange(col, row) %>%
    # x1 and y1 are the cell itself
    dplyr::mutate(x1 = as.numeric(col),
           y1 = as.numeric(row)) %>%
    # y2 goes up to just before the next header in the column
    dplyr::group_by(col) %>%
    dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
    # x2 goes up to just before the next header in any row
    tidyr::nest() %>%
    dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
ABOVE <- function(bag, header, colname, boundaries = NULL) {
  if (is.null(boundaries)) {
    # Join header to cells by proximity
    # First, the domain of each header
    domains <- 
      header %>%
      dplyr::arrange(row, col) %>%
      # y1 is the cell itself
      dplyr::mutate(y1 = as.numeric(row)) %>%
      # x1 and x2 are half-way (rounded down) from the cell to headers either
      # side in the same row
      dplyr::group_by(row) %>%
      dplyr::mutate(
        x1 = floor((col + dplyr::lag(as.numeric(col), default = -Inf) + 2)/2),
        x2 = ceiling((col + dplyr::lead(as.numeric(col), default = Inf) - 2)/2)
      ) %>%
      # y2 goes up to just before the next header in any column
      tidyr::nest() %>%
      dplyr::mutate(y2 = dplyr::lead(as.numeric(row) - 1, default = Inf)) %>%
      tidyr::unnest() %>%
      dplyr::ungroup()
  } else {
    # Domain of each boundary
    boundaries$row <- 1 # universal boundary for every cell in the sheet
    boundary_domains <- NNW_domains(boundaries)
    # Give the headers the same domains, but then limit them by each-other's
    # proximity within the same boundaries.
    domains <- 
      anchor_boundary(header, boundary_domains) %>%
      dplyr::arrange(row, col) %>%
      dplyr::group_by(y1, x1) %>%
      dplyr::mutate(x2 = dplyr::lead(col - 1, default = first(x2))) %>%
      dplyr::ungroup()
  }
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

#' @describeIn join_header Join nearest header in the 'RIGHT' direction.
#' @export
RIGHT <- function(bag, header, colname, boundaries = NULL) {
  if (is.null(boundaries)) {
    # Join header to cells by proximity
    # First, the domain of each header
    domains <- 
      header %>%
      dplyr::arrange(col, row) %>%
      # x2 is the cell itself
      dplyr::mutate(x2 = as.numeric(col)) %>%
      # y1 and y2 are half-way (rounded down) from the cell to headers either
      # side in the same column
      dplyr::group_by(col) %>%
      dplyr::mutate(
        y1 = floor((row + dplyr::lag(as.numeric(row), default = -Inf) + 2)/2),
        y2 = ceiling((row + dplyr::lead(as.numeric(row), default = Inf) - 2)/2)
      ) %>%
      # x1 goes back to just after the previous header in any row
      tidyr::nest() %>%
      dplyr::mutate(x1 = dplyr::lag(as.numeric(col) + 1, default = 1)) %>%
      tidyr::unnest() %>%
      dplyr::ungroup()
  } else {
    # Domain of each boundary
    boundaries$col <- Inf # universal boundary for every cell in the sheet
    boundary_domains <- NNE_domains(boundaries)
    # Give the headers the same domains, but then limit them by each-other's
    # proximity within the same boundaries.
    domains <- 
      anchor_boundary(header, boundary_domains) %>%
      dplyr::arrange(col, row) %>%
      dplyr::group_by(x1, y1) %>%
      dplyr::mutate(y2 = dplyr::lead(row - 1, default = first(y2))) %>%
      dplyr::ungroup()
  }
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

#' @describeIn join_header Join nearest header in the 'BELOW' direction.
#' @export
BELOW <- function(bag, header, colname, boundaries = NULL) {
  if (is.null(boundaries)) {
    # Join header to cells by proximity
    # First, the domain of each header
    domains <- 
      header %>%
      dplyr::arrange(row, col) %>%
      # y2 is the cell itself
      dplyr::mutate(y2 = as.numeric(row)) %>%
      # x1 and x2 are half-way (rounded down) from the cell to headers either
      # side in the same row
      dplyr::group_by(row) %>%
      dplyr::mutate(
        x1 = floor((col + dplyr::lag(as.numeric(col), default = -Inf) + 2)/2),
        x2 = ceiling((col + dplyr::lead(as.numeric(col), default = Inf) - 2)/2)
      ) %>%
      # y1 goes back to just after the previous header in any column
      tidyr::nest() %>%
      dplyr::mutate(y1 = dplyr::lag(as.numeric(row) + 1, default = 1)) %>%
      tidyr::unnest() %>%
      dplyr::ungroup()
  } else {
    # Domain of each boundary
    boundaries$row <- Inf # universal boundary for every cell in the sheet
    boundary_domains <- unpivotr:::SSW_domains(boundaries)
    # Give the headers the same domains, but then limit them by each-other's
    # proximity within the same boundaries.
    domains <- 
      unpivotr:::anchor_boundary(header, boundary_domains) %>%
      dplyr::arrange(row, col) %>%
      dplyr::group_by(y1, x1) %>%
      dplyr::mutate(x2 = dplyr::lead(col - 1, default = first(x2))) %>%
      dplyr::ungroup()
  }
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

#' @describeIn join_header Join nearest header in the 'LEFT' direction.
#' @export
LEFT <- function(bag, header, colname, boundaries = NULL) {
  if (is.null(boundaries)) {
    # Join header to cells by proximity
    # First, the domain of each header
    domains <- 
      header %>%
      dplyr::arrange(col, row) %>%
      # x1 is the cell itself
      dplyr::mutate(x1 = as.numeric(col)) %>%
      # y1 and y2 are half-way (rounded down) from the cell to headers either
      # side in the same column
      dplyr::group_by(col) %>%
      dplyr::mutate(
        y1 = floor((row + dplyr::lag(as.numeric(row), default = -Inf) + 2)/2),
        y2 = ceiling((row + dplyr::lead(as.numeric(row), default = Inf) - 2)/2)
      ) %>%
      # x2 goes up to just before the next header in any row
      tidyr::nest() %>%
      dplyr::mutate(x2 = dplyr::lead(as.numeric(col) - 1, default = Inf)) %>%
      tidyr::unnest() %>%
      dplyr::ungroup()
  } else {
    # Domain of each boundary
    boundaries$col <- 1 # universal boundary for every cell in the sheet
    boundary_domains <- WNW_domains(boundaries)
    # Give the headers the same domains, but then limit them by each-other's
    # proximity within the same boundaries.
    domains <- 
      anchor_boundary(header, boundary_domains) %>%
      dplyr::arrange(col, row) %>%
      dplyr::group_by(x1, y1) %>%
      dplyr::mutate(y2 = dplyr::lead(row - 1, default = first(y2))) %>%
      dplyr::ungroup()
  }
  joined <- anchor_header(bag, domains)
  get_header(bag, joined, colname)
}

anchor_header <- function(bag, domains) {
  # Use data.table non-equi join to join header with cells.
  bag <-
    bag %>%
    dplyr::mutate(row = as.numeric(row), col = as.numeric(col)) # joins with columns that are numeric to allow NA
  bag <- data.table(bag)         # Must be done without %>%
  domains <- data.table(domains) # Must be done without %>%
  bag[domains,
      .(row = x.row, col = x.col, 
        x1 = i.x1, x2 = i.x2, 
        y1 = i.y1, y2 = i.y2,
        header = i.value),
        on = .(row >= y1, row <= y2, col >= x1, col <= x2)] %>%
  dplyr::tbl_df()
}

anchor_boundary <- function(bag, domains) {
  # Use data.table non-equi join to join boundary with cells.
  bag <-
    bag %>%
    dplyr::mutate(row = as.numeric(row), col = as.numeric(col)) # joins with columns that are numeric to allow NA
  bag <- data.table(bag)         # Must be done without %>%
  domains <- data.table(domains) # Must be done without %>%
  bag[domains,
      .(row = x.row, col = x.col, 
        x1 = i.x1, x2 = i.x2, 
        y1 = i.y1, y2 = i.y2,
        header = i.value,
        value = x.value),
        on = .(row >= y1, row <= y2, col >= x1, col <= x2)] %>%
  dplyr::tbl_df()
}

get_header <- function(bag, joined, colname) {
  # Finally, join back on the cells (this step is necessary because data.table
  # returns weird columns from a non-equi join)
  joined %>%
    dplyr::inner_join(bag, by = c("row", "col")) %>%
    dplyr::rename_(.dots = setNames(list(~header), colname)) %>%
    # dplyr::select(-x1, -x2, -y1, -y2)
    dplyr::select(-x1, -x2, -y1, -y2) %>%
    dplyr::select(row, col, dplyr::matches("address"), dplyr::everything())
}
