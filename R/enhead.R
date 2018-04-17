#' Join data cells to headers
#'
#' @description
#' Data cells in a table are associated with header cells by proximity.
#' [enhead()] joins a data frame of data cells to a data frame of header cells,
#' choosing the nearest header cells in the given direction.
#'
#' @param data_cells Data frame of data cells with at least the columns 'row'
#'   and 'column', which are `numeric` or `integer`.
#' @param header_cells Data frame of header cells with at least the columns
#'   'row' and 'column', which are numeric/integer vectors.
#' @param corner_cells Data frame of cells in one row or one column, giving the
#'   corner of the area that a header applies to.  "BELOW" and "LEFT".  For
#'   example, `corner_cells` could be cells with borders on one side.  This is
#'   useful when the nearest header might be the wrong header because it lies on
#'   the other side of a border.
#' @param direction The direction between a data cell and its header, one of
#' `"N"`, `"E"`, `"S"`, `"W"`, `"NNW"`, `"NNE"`, `"ENE"`, `"ESE"`, `"SSE"`,
#' `"SSW"`. `"WSW"`, `"WNW"`, `"ABOVE"`, `"BELOW"`, `"LEFT"` and `"RIGHT"`.  See
#' 'details'.
#' @param drop Logical vector length 1. Whether data cells that can't be
#'   associated with a header should be dropped.  Default: `TRUE`.
#'
#' @details
#' Headers are associated with data by proximity in a given direction.  The
#' directions are mapped to the points of the compass, where 'N' is north (up),
#' 'E' is east (right), and so on.  [enhead()] finds the nearest header to
#' a given data cell in a given direction, and joins it to the data cell.
#'
#' The most common directions to search are `"NNW"` (for left-aligned headers at
#' the top of the table) and `"WNW"` for top-aligned headers at the side of the
#' table.
#'
#' The full list of available directions is `"N"`, `"E"`, `"S"`, `"W"`, `"NNW"`,
#' `"NNE"`, `"ENE"`, `"ESE"`, `"SSE"`, `"SSW"`, `"WSW"`, `"WNW"`, `"ABOVE"`,
#' `"BELOW"`, `"LEFT"`, `"RIGHT"`.  For convenience, these directions are
#' provided as their own functions, wrapping the concept of [enhead()].
#'
#' The difference between `"N"` and `"ABOVE"` (and similar pairs of directions)
#' is that `"N"` finds headers directly above the data cell, whereas `"ABOVE"`
#' matches the nearest header, whether above-left, above-right or directly above
#' the data cell.  This is useful for matching headers that are not aligned to
#' the edge of the data cells that they refer to.  There can be a tie in the
#' directions `"ABOVE"`, `"BELOW"`, `"LEFT"` and `"RIGHT"` , causing `NA`s to be
#' returned in the place of header values.  Provide `corner_cells` to break
#' ties.
#'
#' @name enhead
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
#'   enhead(gender, "NNW") %>%
#'   enhead(satisfaction, "N") %>%
#'   enhead(qualification, "WNW") %>%
#'   enhead(age, "W") %>%
#'   select(-row, -col)
#'
#' # The `drop` argument controls what happens when for some cells there is no
#' # header in the given direction. When `drop = TRUE` (the default), cells that
#' # can't be joined to a header are dropped.  Otherwise they are kept.
#' enhead(datacells, gender, "N")
#' enhead(datacells, gender, "N", drop = FALSE)
enhead <- function(data_cells, header_cells, direction,
                        corner_cells = NULL, drop = TRUE) {
  check_header(header_cells)
  check_direction_enhead(direction)
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(data_cells, header_cells, corner_cells))
  } else if (direction %in% c("N", "E", "S", "W",
                             "NNW", "NNE",
                             "ENE", "ESE",
                             "SSE", "SSW",
                             "WSW", "WNW")) {
    if (!is.null(corner_cells)) {
      stop("'corner_cells' is only supported for the directions 'ABOVE', 'RIGHT'",
           ", 'BELOW' and 'LEFT'.")
    }
    do.call(direction, list(data_cells, header_cells, drop))
  } else {
    stop("The direction ", direction,
         ", is either not recognised or not yet supported.")
  }
}

N <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -row),
                    by = "col",
                    suffix = c(".data", ".header"))
  tibble::as_tibble(out)
}

E <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -col), by = "row",
                    suffix = c(".data", ".header"))
  tibble::as_tibble(out)
}

S <- N
W <- E

NNW <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_left", drop)
}

NNE <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_right", drop)
}

SSE <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_right", drop)
}

SSW <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_left", drop)
}

WNW <- NNW
ENE <- NNE
ESE <- SSE
WSW <- SSW

corner_join <- function(data_cells, header_cells, corner, drop = TRUE) {
  check_header(header_cells)
  headers <-
    partition(header_cells,
              header_cells,
              corner,
              ".partition",
              nest = FALSE,
              strict = FALSE) %>%
    dplyr::select(-row, -col)
  out <-
    partition(data_cells,
              header_cells,
              corner,
              ".partition",
              nest = FALSE,
              strict = FALSE) %>%
    dplyr::inner_join(headers, by = ".partition", suffix = c("", ".y")) %>%
    dplyr::select(-.partition)
  if (!drop) {
    remainder <- dplyr::anti_join(data_cells, out, by = c("row", "col"))
    out <- dplyr::bind_rows(out, remainder)
  }
  out
}

ABOVE <- function(data_cells, header_cells, corner_cells = NULL, drop = TRUE) {
  if (is.null(corner_cells) || min(corner_cells$col) <= min(header_cells$col)) {
    corner <- rlang::quo(NNW)
  } else {
    corner <- rlang::quo(NNE)
  }
  side_join(data_cells, header_cells, !! corner, corner_cells, drop)
}

LEFT <- function(data_cells, header_cells, corner_cells = NULL, drop = TRUE) {
  if (is.null(corner_cells) || min(corner_cells$row) <= min(header_cells$row)) {
    corner <- rlang::quo(WNW)
  } else {
    corner <- rlang::quo(WSW)
  }
  side_join(data_cells, header_cells, !! corner, corner_cells, drop)
}

BELOW <- function(data_cells, header_cells, corner_cells = NULL, drop = TRUE) {
  if (is.null(corner_cells) || min(corner_cells$col) <= min(header_cells$col)) {
    corner <- rlang::quo(SSW)
  } else {
    corner <- rlang::quo(SSE)
  }
  side_join(data_cells, header_cells, !! corner, corner_cells, drop)
}

RIGHT <- function(data_cells, header_cells, corner_cells = NULL, drop = TRUE) {
  if (is.null(corner_cells) || min(corner_cells$row) <= min(header_cells$row)) {
    corner <- rlang::quo(ENE)
  } else {
    corner <- rlang::quo(ESE)
  }
  side_join(data_cells, header_cells, !! corner, corner_cells, drop)
}

side_join <- function(data_cells, header_cells, corner, corner_cells = NULL, drop = TRUE) {
  corner <- rlang::enquo(corner)
  check_header(header_cells)
  if (!is.null(corner_cells)) {
    if (nrow(corner_cells) != nrow(header_cells)) {
      stop("`corner_cells` must have the same number of rows as `header_cells`.")
    }
    header_cells <- dplyr::arrange(header_cells, row, col)
    corner_cells <- dplyr::arrange(corner_cells, row, col)
    header_cells$row <- corner_cells$row
    header_cells$col <- corner_cells$col
  } else {
    corner_text <- rlang::f_text(corner)
    if (corner_text %in% c("NNW", "NNE", "SSW", "SSE")) {
      pos <- rlang::sym("col")
    } else {
      pos <- rlang::sym("row")
    }
    # The domain of each header is up to (but not including) half-way between it
    # and the previous header
    header_cells <- dplyr::mutate(header_cells, !! pos := corner_pos(!! pos, corner))
  }
  rlang::as_function(corner)()(data_cells, header_cells, drop = drop)
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

check_header <- function(header_cells) {
  if (length(unique(header_cells$row)) > 1 & length(unique(header_cells$col)) > 1) {
    stop("Multiple lines of headers are not supported in this way.",
         "\n  Perhaps you meant to concatenate them together first,",
         "\n  Or look at ?partition")
  }
}

# Check that a given direction is a supported compass direction
check_direction_enhead <- function(direction_string) {
  directions <- c("NNW", "N", "NNE",
                  "ENE", "E", "ESE",
                  "SSE", "S", "SSW",
                  "WSW", "W", "WNW",
                  "ABOVE", "LEFT", "RIGHT", "BELOW")
  if (!(direction_string %in% directions)) {
    stop("`direction` must be one of \"",
         paste(directions, collapse = "\", \""),
         "\"")
  }
}
