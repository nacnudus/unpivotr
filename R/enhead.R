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
#' returned in the place of header values.  Avoid ties by using [justify()]
#' first to align header cells to the corner of the data cells they describe.
#'
#' @name enhead
#' @export
#' @examples
#' library(dplyr)
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- as_cells(x)
#' cells <- cells[!is.na(cells$chr), ]
#' head(cells)
#' # Select the cells containing the values
#' data_cells <-
#'   filter(cells, row >= 3, col >= 3) %>%
#'   transmute(row, col, count = as.integer(chr))
#' head(data_cells)
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
#' data_cells %>%
#'   enhead(gender, "NNW") %>%
#'   enhead(satisfaction, "N") %>%
#'   enhead(qualification, "WNW") %>%
#'   enhead(age, "W") %>%
#'   select(-row, -col)
#'
#' # The `drop` argument controls what happens when for some cells there is no
#' # header in the given direction. When `drop = TRUE` (the default), cells that
#' # can't be joined to a header are dropped.  Otherwise they are kept.
#' enhead(data_cells, gender, "N")
#' enhead(data_cells, gender, "N", drop = FALSE)
enhead <- function(data_cells, header_cells, direction, drop = TRUE) {
  UseMethod("enhead")
}

#' @export
enhead.data.frame <- function(data_cells, header_cells, direction,
                              drop = TRUE) {
  check_header(header_cells)
  check_direction_enhead(direction)
  check_distinct(data_cells)
  check_distinct(header_cells)
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(data_cells, header_cells))
  } else if (direction %in% c(
    "N", "E", "S", "W",
    "NNW", "NNE",
    "ENE", "ESE",
    "SSE", "SSW",
    "WSW", "WNW"
  )) {
    do.call(direction, list(data_cells, header_cells, drop))
  }
}

N <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -row),
    by = "col",
    suffix = c(".data", ".header")
  )
  tibble::as_tibble(out)
}

E <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -col),
    by = "row",
    suffix = c(".data", ".header")
  )
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
    header_cells %>%
    partition(dplyr::distinct(header_cells, row, col),
      corner,
      nest = FALSE
    ) %>%
    dplyr::select(-row, -col)
  datas <- partition(data_cells,
    dplyr::distinct(header_cells, row, col),
    corner,
    nest = FALSE,
    strict = FALSE
  )
  out <-
    dplyr::inner_join(datas, headers,
      by = c("corner_row", "corner_col"),
      suffix = c("", ".y")
    ) %>%
    dplyr::select(-corner_row, -corner_col)
  if (!drop) {
    remainder <- dplyr::anti_join(data_cells, out, by = c("row", "col"))
    out <- dplyr::bind_rows(out, remainder)
  }
  out
}

ABOVE <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "NNW", drop)
}

LEFT <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "WNW", drop)
}

BELOW <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "SSW", drop)
}

RIGHT <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "ENE", drop)
}

side_join <- function(data_cells, header_cells, corner, drop = TRUE) {
  check_header(header_cells)
  if (corner %in% c("NNW", "NNE", "SSW", "SSE")) {
    pos <- rlang::sym("col")
  } else {
    pos <- rlang::sym("row")
  }
  # The domain of each header is up to (but not including) half-way between it
  # and the previous header
  header_cells <- dplyr::mutate(
    header_cells,
    !!pos := corner_pos(!!pos, corner)
  )
  rlang::as_function(corner)(data_cells, header_cells, drop = drop)
}

corner_pos <- function(cells, corner) {
  corner_names <- c("NNW", "NNE", "ENE", "ESE", "SSE", "SSW", "WSW", "WNW")
  corner_poss <- rep(c("col", "col", "row", "row"), 2L)
  corner_looks <- c(
    rep(c(dplyr::lag, dplyr::lead), 2L),
    rep(c(dplyr::lead, dplyr::lag), 2L)
  )
  corner_defaults <- c(1L, 16384L, 1L, 1048576L, 16384L, 1L, 1048576L, 1L)
  corner_coefs <- c(2L, -2L, 2L, -2L, -2L, 2L, -2L, 2L)
  corner_extremes <- c(
    rep(c(floor, ceiling), 2L),
    rep(c(ceiling, floor), 2L)
  )
  corner_i <- match(corner, corner_names)
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
    stop(
      "Multiple lines of headers are not supported in this way.",
      "\n  Perhaps you meant to concatenate them together first,",
      "\n  Or look at ?partition"
    )
  }
}

# Check that a given direction is a supported compass direction
check_direction_enhead <- function(direction_string) {
  directions <- c(
    "NNW", "N", "NNE",
    "ENE", "E", "ESE",
    "SSE", "S", "SSW",
    "WSW", "W", "WNW",
    "ABOVE", "LEFT", "RIGHT", "BELOW"
  )
  if (!(direction_string %in% directions)) {
    stop(
      "`direction` must be one of \"",
      paste(directions, collapse = "\", \""),
      "\""
    )
  }
}

check_distinct <- function(cells) {
  if (dplyr::n_distinct(dplyr::select(cells, row, col)) != nrow(cells)) {
    stop("Row and column numbers must be distinct.",
      "\n  Perhaps you meant to use a single sheet.",
      call. = FALSE
    )
  }
}
