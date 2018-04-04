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
#' @param corners Data frame. Only applies to the directions "ABOVE", "RIGHT",
#' "BELOW" and "LEFT".  A bag of cells in one row or one column, giving the
#' corner of the area that a header applies to.  For example, `corners` could
#' be a bag of cells with borders on one side.  This is useful when the nearest
#' header might be the wrong header because it lies on the other side of a
#' border.
#' @param direction The name of a function that joins headers to data cells, one
#' of `N`, `E`, `S`, `W`, `NNW`, `NNE`, `ENE`, `ESE`, `SSE`, `SSW`. `WSW`,
#' `WNW`, `ABOVE`, `BELOW`, `LEFT` and `RIGHT`.  See 'details'.
#' @param drop Logical vector length 1. Whether data cells that can't be
#' associated with a header should be dropped.  Default: TRUE.
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
#'   filter(cells, row >= 3, col >= 3) %>%
#'   transmute(row, col, count = as.integer(chr))
#' head(datacells)
#' # Select the headers
#' qualification <-
#'   filter(cells, col == 1) %>%
#'   select(row, col, qualification = chr)
#' age <-
#'   filter(cells, col == 2) %>%
#'   select(row, col, age = chr)
#' gender <-
#'   filter(cells, row == 1) %>%
#'   select(row, col, gender = chr)
#' satisfaction <-
#'   filter(cells, row == 2) %>%
#'   select(row, col, satisfaction = chr)
#' # From each data cell, search for the nearest one of each of the headers
#' datacells %>%
#'   NNW(gender) %>%
#'   N(satisfaction) %>%
#'   WNW(qualification) %>%
#'   W(age) %>%
#'   select(-row, -col)
#'
#' # The `drop` argument controls what happens when for some cells there is no
#' # header in the given direction. When `drop = TRUE` (the default), cells that
#' # can't be joined to a header are dropped.  Otherwise they are kept.
#' N(datacells, gender)
#' N(datacells, gender, drop = FALSE)
join_header <- function(bag, header, direction, corners = NULL, drop = TRUE) {
  direction <- rlang::quo_name(rlang::enexpr(direction))
  check_header(header)
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(bag, header, corners))
  } else if (direction %in% c("N", "E", "S", "W",
                             "NNW", "NNE",
                             "ENE", "ESE",
                             "SSE", "SSW",
                             "WSW", "WNW")) {
    if (!is.null(corners)) {
      stop("'corners' is only supported for the directions 'ABOVE', 'RIGHT'",
           ", 'BELOW' and 'LEFT'.")
    }
    do.call(direction, list(bag, header, drop))
  } else {
    stop("The direction ", direction,
         ", is either not recognised or not yet supported.")
  }
}

#' @describeIn join_header Join nearest header in the 'N' direction.
#' @export
N <- function(bag, header, drop = TRUE) {
  check_header(header)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(bag, dplyr::select(header, -row),
                    by = "col",
                    suffix = c(".data", ".header"))
  tibble::as_tibble(out)
}

#' @describeIn join_header Join nearest header in the 'E' direction.
#' @export
E <- function(bag, header, drop = TRUE) {
  check_header(header)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(bag, dplyr::select(header, -col), by = "row",
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
NNW <- function(bag, header, drop = TRUE) {
  corner_join(bag, header, "top_left", drop)
}

#' @describeIn join_header Join nearest header in the 'NNE' direction.
#' @export
NNE <- function(bag, header, drop = TRUE) {
  corner_join(bag, header, "top_right", drop)
}

#' @describeIn join_header Join nearest header in the 'SSE' direction.
#' @export
SSE <- function(bag, header, drop = TRUE) {
  corner_join(bag, header, "bottom_right", drop)
}

#' @describeIn join_header Join nearest header in the 'SSW' direction.
#' @export
SSW <- function(bag, header, drop = TRUE) {
  corner_join(bag, header, "bottom_left", drop)
}

#' @describeIn join_header Join nearest header in the 'WNW' direction.
#' @export
WNW <- NNW

#' @describeIn join_header Join nearest header in the 'ENE' direction.
#' @export
ENE <- NNE

#' @describeIn join_header Join nearest header in the 'ESE' direction.
#' @export
ESE <- SSE

#' @describeIn join_header Join nearest header in the 'WSW' direction.
#' @export
WSW <- SSW

corner_join <- function(bag, header, corner, drop = TRUE) {
  headers <-
    partition(header, header, corner, ".partition") %>%
    dplyr::select(-row, -col)
  data_cells <-
    partition(bag, header, corner, ".partition") %>%
    dplyr::inner_join(headers, by = ".partition", suffix = c("", ".y")) %>%
    dplyr::select(-.partition)
  if (!drop) {
    remainder <- dplyr::anti_join(bag, data_cells, by = c("row", "col"))
    data_cells <- dplyr::bind_rows(data_cells, remainder)
  }
  data_cells
}

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
ABOVE <- function(bag, header, corners = NULL, drop = TRUE) {
  if (is.null(corners) || min(corners$col) <= min(header$col)) {
    corner <- rlang::quo(NNW)
  } else {
    corner <- rlang::quo(NNE)
  }
  side_join(bag, header, !! corner, corners, drop)
}

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
LEFT <- function(bag, header, corners = NULL, drop = TRUE) {
  if (is.null(corners) || min(corners$row) <= min(header$row)) {
    corner <- rlang::quo(WNW)
  } else {
    corner <- rlang::quo(WSW)
  }
  side_join(bag, header, !! corner, corners, drop)
}

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
BELOW <- function(bag, header, corners = NULL, drop = TRUE) {
  if (is.null(corners) || min(corners$col) <= min(header$col)) {
    corner <- rlang::quo(SSW)
  } else {
    corner <- rlang::quo(SSE)
  }
  side_join(bag, header, !! corner, corners, drop)
}

#' @describeIn join_header Join nearest header in the 'ABOVE' direction.
#' @export
RIGHT <- function(bag, header, corners = NULL, drop = TRUE) {
  if (is.null(corners) || min(corners$row) <= min(header$row)) {
    corner <- rlang::quo(ENE)
  } else {
    corner <- rlang::quo(ESE)
  }
  side_join(bag, header, !! corner, corners, drop)
}

side_join <- function(bag, header, corner, corners = NULL, drop = TRUE) {
  corner <- rlang::enquo(corner)
  unpivotr:::check_header(header)
  if (!is.null(corners)) {
    if (nrow(corners) != nrow(header)) {
      stop("`corners` must have the same number of rows as `header`.")
    }
    header <- dplyr::arrange(header, row, col)
    corners <- dplyr::arrange(corners, row, col)
    header$row <- corners$row
    header$col <- corners$col
  } else {
    corner_text <- rlang::f_text(corner)
    if (corner_text %in% c("NNW", "NNE", "SSW", "SSE")) {
      pos <- rlang::sym("col")
    } else {
      pos <- rlang::sym("row")
    }
    # The domain of each header is up to (but not including) half-way between it
    # and the previous header
    header <- dplyr::mutate(header, !! pos := corner_pos(!! pos, corner))
  }
  rlang::as_function(corner)()(bag, header, drop = drop)
}

corner_pos <- function(cells, corner) {
  corner_names <- c("NNW", "NNE", "ENE", "ESE", "SSE", "SSW", "WSW", "WNW")
  corner_poss <- rep(c("col", "col", "row", "row"), 2L)
  corner_looks <- c(rep(c(dplyr::lag, dplyr::lead), 2L),
                    rep(c(dplyr::lead, dplyr::lag), 2L))
  corner_defaults <- c(1L, 16384L, 1L, 1048576L, 16384L, 1L, 1048576L, 1L)
  corner_coefs <- c(2L, -2L, 2L, -2L, -2L, 2L, -2L, 2L)
  corner_extremes <- c(rep(c(floor, ceiling), 2L),
                       rep(c(ceiling, floor), 2L))
  corner_i <- match(rlang::f_text(corner), corner_names)
  pos <- rlang::sym(corner_poss[corner_i])
  look <- rlang::as_function(corner_looks[[corner_i]], ns_env("dplyr"))
  default <- corner_defaults[corner_i]
  extreme <- corner_extremes[[corner_i]]
  coef <- corner_coefs[corner_i]
  out <- extreme((cells + look(cells) + coef) / 2)
  out[is.na(out)] <- default
  out
}

check_header <- function(header) {
  if (length(unique(header$row)) > 1 & length(unique(header$col)) > 1) {
    stop("Multiple lines of headers are not supported in this way.",
         "\n  Perhaps you meant to concatenate them together first,",
         "\n  Or look at the examples in",
         " `vignette(\"small-multiples\", package = \"unpivotr\")`.")
  }
}
