#' Join data cells to headers
#'
#' @description
#' Data cells in a table are associated with header cells by proximity.
#' [enhead()] joins a data frame of data cells to a data frame of header cells,
#' choosing the nearest header cells in the given direction.  See `?direction`.
#'
#' @param data_cells Data frame of data cells with at least the columns 'row'
#'   and 'column', which are `numeric` or `integer`.
#' @param header_cells Data frame of header cells with at least the columns
#'   'row' and 'column', which are numeric/integer vectors.
#' @param direction The direction between a data cell and its header, one of
#' `"up"`, `"right"`, `"down"`, `"left"`, `"up-left"`, `"up-right"`,
#' `"right-up"`, `"right-down"`, `"down-right"`, `"down-left"`, `"left-down"`,
#' `"left-up"`, `"up-ish"`, `"down-ish"`, `"left-ish"` and `"right-ish"`. See
#' `?direction`.
#' @param drop Logical vector length 1. Whether data cells that can't be
#'   associated with a header should be dropped.  Default: `TRUE`.
#'
#' @name enhead
#' @export
#' @examples
#' library(dplyr)
#' # Load some pivoted data
#' (x <- purpose$`up-left left-up`)
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
#'   enhead(gender, "up-left") %>%
#'   enhead(satisfaction, "up") %>%
#'   enhead(qualification, "left-up") %>%
#'   enhead(age, "left") %>%
#'   select(-row, -col)
#'
#' # The `drop` argument controls what happens when for some cells there is no
#' # header in the given direction. When `drop = TRUE` (the default), cells that
#' # can't be joined to a header are dropped.  Otherwise they are kept.
#' enhead(data_cells, gender, "up")
#' enhead(data_cells, gender, "up", drop = FALSE)
enhead <- function(data_cells, header_cells, direction, drop = TRUE) {
  UseMethod("enhead")
}

#' @export
enhead.data.frame <- function(data_cells, header_cells, direction,
                              drop = TRUE) {
  check_header(header_cells)
  direction <- standardise_direction(direction)
  check_distinct(data_cells)
  check_distinct(header_cells)
  if (direction %in% c("up-ish", "right-ish", "down-ish", "left-ish")) {
    do.call(direction, list(data_cells, header_cells))
  } else if (direction %in% c(
    "up", "right", "down", "left",
    "up-left", "up-right",
    "right-up", "right-down",
    "down-right", "down-left",
    "left-down", "left-up"
  )) {
    do.call(direction, list(data_cells, header_cells, drop))
  }
}

up <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -row),
    by = "col",
    suffix = c(".data", ".header")
  )
  tibble::as_tibble(out)
}

right <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -col),
    by = "row",
    suffix = c(".data", ".header")
  )
  tibble::as_tibble(out)
}

down <- up
left <- right

`up-left` <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_left", drop)
}

`up-right` <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_right", drop)
}

`down-right` <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_right", drop)
}

`down-left` <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_left", drop)
}

`left-up` <- `up-left`
`right-up` <- `up-right`
`right-down` <- `down-right`
`left-down` <- `down-left`

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

`up-ish` <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "up-left", drop)
}

`left-ish` <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "left-up", drop)
}

`down-ish` <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "down-left", drop)
}

`right-ish` <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "right-up", drop)
}

side_join <- function(data_cells, header_cells, corner, drop = TRUE) {
  check_header(header_cells)
  if (corner %in% c("up-left", "up-right", "down-left", "down-right")) {
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
  corner_names <-
    c("up-left", "up-right", "right-up", "right-down",
      "down-right", "down-left", "left-down", "left-up")
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

check_distinct <- function(cells) {
  if (dplyr::n_distinct(dplyr::select(cells, row, col)) != nrow(cells)) {
    stop("Row and column numbers must be distinct.",
      "\n  Perhaps you meant to use a single sheet.",
      call. = FALSE
    )
  }
}
