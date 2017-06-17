#' Join a bag of data cells to some header, by proximity in a given direction,
#' e.g. NNW searches up and up-left from a data cell to find a header cell.
#'
#' @description A bag of data cells is a data frame with at least the columns
#' 'row' and 'col', as well as any others that carry information about the
#' cells, e.g. their values.  Cells in a table are associated with header cells
#' by proximity.  Having collected header cells and data cells into separate
#' data frames, 'join_header' and the related functions 'NNW', 'ABOVE', etc.,
#' join the values in the header cells to the data cells, choose the nearest
#' header to each cell, in a given direction.
#' @param bag Data frame. A bag of data cells including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param header Data frame. A bag of data cells including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param boundaries Data frame. Only applies to the directions "ABOVE",
#' "RIGHT", "BELOW" and "LEFT".  A bag of cells in one row or one column,
#' demarking boundaries within which to match headers with cells.  For example,
#' a boundary could be a bag of cells with borders on one side.  This is useful
#' when the nearest header might be the wrong header because it lies on the
#' other side of a border.
#' @param direction Character vector length 1. A compass direction to search for
#' the nearest header.  See 'details'.
#' @details Headers are associated with data by proximity in a given direction.
#' The directions are mapped to the points of the compass, where 'N' is north
#' (up), 'E' is east (right), and so on.  `join_header()` finds the nearest
#' header to a given data cell in a given direction, and joins its value to the
#' data cell.  The most common directions to search are 'NNW' (for left-aligned
#' headers at the top of the table) and 'WNW' for top-aligned headers at the
#' side of the table.  The difference between 'N' and 'ABOVE' (and similar pairs
#' of directions) is that 'N' finds headers directly above the data cell,
#' whereas 'ABOVE' matches the nearest header, whether above-left, above-right
#' or directly above the data cell.  This is useful for matching headers that
#' are not aligned to the edge of the data cells that they refer to.  There can
#' be a tie in the directions 'ABOVE', 'BELOW', 'LEFT' and 'RIGHT' , causing NAs
#' to be returned in the place of header values.  The full list of available
#' directions is 'N', 'E', 'S', 'W', 'NNW', 'NNE', 'ENE', 'ESE', 'SSE', 'SSW',
#' 'WSW', 'WNW', 'ABOVE', 'BELOW', 'LEFT', 'RIGHT'.  For convenience, these
#' directions are provided as their own functions, wrapping the concept of
#' 'join_header()'.
#' @name join_header
#' @export
#' @examples
#' library(dplyr)
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- tidy_table(x)
#' cells <- cells[!is.na(cells$chr), ]
#' head(cells)
#' # Select the cells containing the values
#' datacells <-
#'   cells %>%
#'   filter(row >= 3, col >= 3)
#' head(datacells)
#' # Select the row headers
#' row_headers <-
#'   cells %>%
#'   filter(col <= 2) %>%
#'   select(row, col, header = chr) %>%
#'   split(.$col) # Separate each column of headers
#' row_headers
#' # Select the column headers
#' col_headers <-
#'   cells %>%
#'   filter(row <= 2) %>%
#'   select(row, col, header = chr) %>%
#'   split(.$row) # Separate each row of headers
#' col_headers
#' # From each data cell, search for the nearest one of each of the headers
#' datacells %>%
#'   NNW(col_headers$`1`) %>%
#'   N(col_headers$`2`) %>%
#'   WNW(row_headers$`1`) %>%
#'   W(row_headers$`2`)
join_header <- function(bag, header, direction, boundaries = NULL) {
  check_header(header)
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(bag, header, boundaries))
  } else if (direction %in% c("N", "E", "S", "W",
                             "NNW", "NNE",
                             "ENE", "ESE",
                             "SSE", "SSW",
                             "WSW", "WNW")) {
    if (!is.null(boundaries)) {
      stop("'boundaries' is only supported for the directions 'ABOVE', 'RIGHT'",
           ", 'BELOW' and 'LEFT'.")
    }
    do.call(direction, list(bag, header))
  } else {
    stop("The direction ", direction,
         ", is either not recognised or not yet supported.")
  }
}

#' @describeIn join_header Join nearest header in the 'N' direction.
#' @export
N <- function(bag, header) {
  check_header(header)
  out <- inner_join(bag, select(header, -row),
                    by = "col",
                    suffix = c(".data", ".header"))
  tibble::as_tibble(out)
}

#' @describeIn join_header Join nearest header in the 'E' direction.
#' @export
E <- function(bag, header) {
  check_header(header)
  out <- inner_join(bag, select(header, -col), by = "row",
                    suffix = c(".data", ".header"))
  tibble::as_tibble(out)
}

#' @describeIn join_header Join nearest header in the 'S' direction.
#' @export
S <- N

#' @describeIn join_header Join nearest header in the 'W' direction.
#' @export
W <- E

#' @describeIn join_header Join nearest header in the 'NNW' direction.
#' @export
NNW <- function(bag, header) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-row) %>%
    dplyr::arrange(col) %>%
    dplyr::mutate(col = as.double(col),
                  to_col = dplyr::lead(col - 1, default = Inf))
  header <- data.table::data.table(header) # Must be done without %>%
  bag$row <- as.double(bag$row) # Required for data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag)       # Must be done without %>%
  header[bag, on = .(col <= col, to_col >= col)] %>% # Left-join (bag is left)
    dplyr::tbl_df() %>%
    select(-to_col) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'NNE' direction.
#' @export
NNE <- function(bag, header) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-row) %>%
    dplyr::arrange(col) %>%
    dplyr::mutate(col = as.double(col),
                  from_col = dplyr::lag(col + 1, default = -Inf))
  header <- data.table::data.table(header) # Must be done without %>%
  bag$row <- as.double(bag$row) # Required for data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag)       # Must be done without %>%
  header[bag, on = .(from_col <= col, col >= col)] %>% # Left-join (bag is left)
    dplyr::tbl_df() %>%
    select(-from_col) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'ENE' direction.
#' @export
ENE <- function(bag, header) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-col) %>%
    dplyr::arrange(row) %>%
    dplyr::mutate(row = as.double(row),
                  to_row = dplyr::lead(row - 1, default = Inf))
  header <- data.table::data.table(header) # Must be done without %>%
  bag$row <- as.double(bag$row) # Required for data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag)       # Must be done without %>%
  header[bag, on = .(row <= row, to_row >= row)] %>% # Left-join (bag is left)
    dplyr::tbl_df() %>%
    select(-to_row) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'ESE' direction.
#' @export
ESE <- function(bag, header) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-col) %>%
    dplyr::arrange(row) %>%
    dplyr::mutate(row = as.double(row),
                  from_row = dplyr::lag(row + 1, default = -Inf))
  header <- data.table::data.table(header) # Must be done without %>%
  bag$row <- as.double(bag$row) # Required for data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag)       # Must be done without %>%
  header[bag, on = .(from_row <= row, row >= row)] %>% # Left-join (bag is left)
    dplyr::tbl_df() %>%
    select(-from_row) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'SSE' direction.
#' @export
SSE <- NNE

#' @describeIn join_header Join nearest header in the 'SSW' direction.
#' @export
SSW <- NNW

#' @describeIn join_header Join nearest header in the 'WSW' direction.
#' @export
WSW <- ESE

#' @describeIn join_header Join nearest header in the 'WNW' direction.
#' @export
WNW <- ENE

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
ABOVE <- function(bag, header, boundaries = NULL) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-row) %>%
    dplyr::arrange(col)
  if (!is.null(boundaries)) {
    # Align the headers to the boundaries.
    # A boundary is marked at col=1 if it doesn't already exist.  There may then
    # be fewer headers than boundaries, but not more headers than boundaries.
    boundaries <-
      boundaries %>%
      dplyr::arrange(col) %>%
      dplyr::mutate(to_col = dplyr::lead(col - 1, default = Inf)) %>%
      dplyr::select(col, to_col) %>%
      dplyr::rename(from_col = col)
    boundaries <- data.table::data.table(boundaries) # Must be done without %>%
    header <- # Rename columns to avoid misleading data.table renaming
      header %>%
      rename(from_col = col) %>%
      mutate(from_col = as.double(from_col), to_col = from_col) # For data.table join on Inf
    header <- data.table::data.table(header) # Must be done without %>%
    header <- header[boundaries, on = .(from_col >= from_col, to_col <= to_col)] # Left-join (boundaries is left)
    # Boundaries without headers still exist but are NA
    if (any(diff(header$from_col) == 0)) {
      stop("Multiple headers were detected within the same pair of boundaries.",
           "\n  Please provide boundaries to separate every header.")
    }
  } else {
    # The domain of each header is up to (but not including) half-way between it
    # and headers either side, except the ends, which extend to the edge of the
    # sheet.
    header <-
      header %>%
      dplyr::mutate(
        from_col = floor((col + dplyr::lag(as.numeric(col), default = -Inf) + 2)/2),
        to_col = ceiling((col + dplyr::lead(as.numeric(col), default = Inf) - 2)/2)
      ) %>%
      select(-col)
    header <- data.table::data.table(header) # Must be done without %>%
  }
  bag$row <- as.double(bag$row) # For data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag) # Must be done without %>%
  header[bag, on = .(from_col <= col, to_col >= col)] %>%
    dplyr::tbl_df() %>%
    rename(col = from_col) %>%
    select(-to_col) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'LEFT' direction.
#' @export
LEFT <- function(bag, header, boundaries = NULL) {
  check_header(header)
  header <-
    header %>%
    dplyr::select(-col) %>%
    dplyr::arrange(row)
  if (!is.null(boundaries)) {
    # Align the headers to the boundaries.
    # A boundary is marked at row=1 if it doesn't already exist.  There may then
    # be fewer headers than boundaries, but not more headers than boundaries.
    boundaries <-
      boundaries %>%
      dplyr::arrange(row) %>%
      dplyr::mutate(to_row = dplyr::lead(row - 1, default = Inf)) %>%
      dplyr::select(row, to_row) %>%
      dplyr::rename(from_row = row)
    boundaries <- data.table::data.table(boundaries) # Must be done without %>%
    header <- # Rename rowumns to avoid misleading data.table renaming
      header %>%
      rename(from_row = row) %>%
      mutate(to_row = from_row) %>%
      mutate(from_row = as.double(from_row), to_row = from_row) # For data.table join on Inf
    header <- data.table::data.table(header) # Must be done without %>%
    header <- header[boundaries, on = .(from_row >= from_row, to_row <= to_row)] # Left-join (boundaries is left)
    # Boundaries without headers still exist but are NA
    if (any(diff(header$from_row) == 0)) {
      stop("Multiple headers were detected within the same pair of boundaries.",
           "\n  Please provide boundaries to separate every header.")
    }
  } else {
    # The domain of each header is up to (but not including) half-way between it
    # and headers either side, except the ends, which extend to the edge of the
    # sheet.
    header <-
      header %>%
      dplyr::mutate(
        from_row = floor((row + dplyr::lag(as.numeric(row), default = -Inf) + 2)/2),
        to_row = ceiling((row + dplyr::lead(as.numeric(row), default = Inf) - 2)/2)
      ) %>%
      select(-row)
    header <- data.table::data.table(header) # Must be done without %>%
  }
  bag$row <- as.double(bag$row) # For data.table join on Inf
  bag$col <- as.double(bag$col)
  bag <- data.table::data.table(bag) # Must be done without %>%
  header[bag, on = .(from_row <= row, to_row >= row)] %>%
    dplyr::tbl_df() %>%
    rename(row = from_row) %>%
    select(-to_row) %>%
    mutate(row = as.integer(row), col = as.integer(col)) %>%
    select_(.dots = as.list(c(colnames(bag), everything(.)))) %>%
    tibble::as_tibble()
}

#' @describeIn join_header Join nearest header in the 'BELOW' direction.
#' @export
BELOW <- ABOVE

#' @describeIn join_header Join nearest header in the 'BELOW' direction.
#' @export
RIGHT <- LEFT

check_header <- function(header) {
  if (length(unique(header$row)) > 1 & length(unique(header$col)) > 1) {
    stop("Multiple lines of headers are not supported in this way.",
         "\n  Perhaps you meant to concatenate them together first,",
         "\n  Or look at the examples in",
         " `vignette(\"small-multiples\", package = \"unpivotr\")`.")
  }
}

