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
#' @param cells Data frame. All the cells in the sheet, among which to offset
#' the bag (extensions beyond existing cells will be padded with blank cells).
#' Must include at least the columns 'row' and 'column', as well as any columns
#' referred to by the boundary formula.
#' @param direction Character vector length 1. The direction in which to offset,
#' among the compass directions "N", "E", "S", "W", where "N" is north (up).
#' @param n Integer vector length 1, >= 0. The number of rows/cols to offset by
#' in the given direction.
#' @param boundary Formula to express a boundary condition.  `~ col <= 50` would
#' go up to the 50th column.  NAs are treated the same as FALSE, but with a
#' warning.
#' @param include Logical vector length 1. Whether to include in the extension
#' the first cell (and its row/col of fellow cells) at which the boundary
#' condition is met.
#' @param edge Logical vector length 1. Whether to require the boundary formula
#' to be TRUE along the entire leading edge of the bag that is being offset
#' @details A bag may have ragged rows or ragged cols. Gaps will be filled in,
#' even when n = 0.
#' @export
#' @examples
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- tidytable(x, rownames = FALSE, colnames = FALSE)
#' cells <- cells[!is.na(cells$character), ] # Introduce 'holes' in the data
#' # Select an L-shape with gaps
#' bag <- dplyr::filter(cells, row %in% 3:4, col %in% 1:2)
#' # Offset, notice the L has been squared-off (padded)
#' offset_N(bag, cells, 1)
#' # Select a particular cell
#' cell <- cells[which(cells$row == 3 & cells$col == 3), ]
#' # Offset the selection downwards, stopping before the NA.
#' offset_S(cell, cells, boundary = ~ is.na(character))
#' # Offset the selection right, up to and including the fifth column.
#' offset_E(cell, cells, boundary = ~ col == 5, include = TRUE)
#' # Offset the selection beyond the existing cells
#' offset_E(cell, cells, 15)
#' # This doesn't work inside formulas, because it would mean testing the
#' # boundary formula on every possible cell in the given direction
#' \dontrun{offset_E(cell, cells, boundary = ~ col == 15)}
#' cell <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
#' offset_N(cell, cells, boundary = ~ !is.na(character), edge = TRUE)
offset <- function(bag, cells, direction, n = NULL, boundary = NULL,
                   edge = FALSE, include = FALSE) {
  test_offset_args(bag, direction, n, boundary, edge, include)
  if (!is.null(n)) {
    if (n == 0) {return(bag)}
    n <- as.integer(n) # Prevents coercion of row/col to double
    offset_n(bag, cells, direction, n)
  } else {
    offset_boundary(bag, cells, direction, boundary, edge, include)
  }
}

#' @describeIn offset Offset a bag of cells to the north
#' @export
offset_N <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  offset(bag, cells, "N", n, boundary, edge, include)
}

#' @describeIn offset Offset a bag of cells to the east
#' @export
offset_E <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  offset(bag, cells, "E", n, boundary, edge, include)
}

#' @describeIn offset Offset a bag of cells to the south
#' @export
offset_S <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  offset(bag, cells, "S", n, boundary, edge, include)
}

#' @describeIn offset Offset a bag of cells to the west
#' @export
offset_W <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  offset(bag, cells, "W", n, boundary, edge, include)
}

test_offset_args <- function(bag, direction, n, boundary, edge, include) {
  if (nrow(bag) == 0) {
    stop("Cannot offset an empty bag ('bag' has no rows)")
  }
  # Tests arguments to `offset_` functions.
  if (!(direction %in% c("N", "S", "E", "W"))) {
    stop("'direction' must be one of 'N', 'S', 'E' and 'W'")
  }
  if (!xor(is.null(n), is.null(boundary))) {
    stop("Exactly one of 'n' and 'boundary' must be specified")
  }
  if (!is.null(n)) {
    if (is(n, "formula")) {
      stop("'n' must be numeric; did you intend to use 'boundary'?")
    }
    if (!is(n, "numeric")) {
      stop("'n' must be numeric.")
    } else {
      if (n < 0) {stop("'n' must be >= 0")}
      if (n %% 1 != 0) {stop("'n' must be a whole number (e.g. 1, 1L, 1.0)")}
    }
  }
  if (is.null(boundary) && (edge || include)) {
    stop("'edge' and 'include' only apply when 'boundary' is specified")
  }
}

offset_n <- function(bag, cells, direction, n) {
  if (direction == "N") {
    rows <- range(bag$row) - n
    cols <- range(bag$col)
  }
  if (direction == "E") {
    rows <- range(bag$row)
    cols <- range(bag$col) + n
  }
  if (direction == "S") {
    rows <- range(bag$row) + n
    cols <- range(bag$col)
  }
  if (direction == "W") {
    rows <- range(bag$row)
    cols <- range(bag$col) - n
  }
  rows <- rows[rows >= 1L]
  cols <- cols[cols >= 1L]
  if (length(rows) == 0 | length(cols) == 0) {
    stop("The offset went off the edge of the spreadsheet (row or col <= 0).")
  }
  pad <- tidyr::crossing(row = tidyr::full_seq(rows, 1L),
                         col = tidyr::full_seq(cols, 1L))
  dplyr::left_join(pad, cells, by = c("row", "col"))
}

offset_boundary <- function(bag, cells, direction, boundary, edge, include) {
  if (direction == "N") {
    rowcol_function <- max
    rowcol_function_opposite <- min
    rowcol_formula <- ~ row
    lt_gt <- `>`
    y1 <- max(min(cells$row) - 1L, 1L)
    y2 <- min(bag$row) - 1L
    x1 <- min(bag$col)
    x2 <- max(bag$col)
  }
  if (direction == "E") {
    rowcol_function <- min
    rowcol_function_opposite <- max
    rowcol_formula <- ~ col
    lt_gt <- `<`
    y1 <- min(bag$row)
    y2 <- max(bag$row)
    x1 <- max(bag$col) + 1L
    x2 <- max(cells$col) + 1L
  }
  if (direction == "S") {
    rowcol_function <- min
    rowcol_function_opposite <- max
    rowcol_formula <- ~ row
    lt_gt <- `<`
    y1 <- max(bag$row) + 1L
    y2 <- max(cells$row) + 1L
    x1 <- min(bag$col)
    x2 <- max(bag$col)
  }
  if (direction == "W") {
    rowcol_function <- max
    rowcol_function_opposite <- min
    rowcol_formula <- ~ col
    lt_gt <- `>`
    y1 <- min(bag$row)
    y2 <- max(bag$row)
    x1 <- max(min(cells$col) - 1L, 1L)
    x2 <- min(bag$col) - 1L
  }
  rowcol_name <- rowcol_formula[[2]] # rhs of rowcol_formula (min or max)
  rowcol_text <- deparse(rowcol_name) # "row" or "col"
  cells <- 
    cells %>%
    dplyr::filter(row >= y1,
                  row <= y2,
                  col >= x1,
                  col <= x2) %>%
    pad(c(y1, y2), c(x1, x2)) %>% # Pad with blanks for boundary formula's sake
    dplyr::mutate_(.boundary = boundary) # Apply the boundary formula
  if (edge) {
    # Filter for edges where the boundary exists in every row/col
    boundaries <- 
      cells %>%
      dplyr::group_by_(rowcol_text) %>%
      dplyr::summarise(.boundary = all(.boundary)) %>%
      dplyr::filter(.boundary) %>% .[[rowcol_text]]
  } else {
    # Get all individual boundaries
    boundaries <- 
      dplyr::filter(cells, .boundary) %>% .[[rowcol_text]]
  }
  if (length(boundaries) == 0) {
    stop("No boundary detected")
  }
  # Get cells up to the nearest boundary in any row/col
  near_boundary <- rowcol_function(boundaries)
  cells <-
    cells %>%
    dplyr::select(-.boundary) %>%
    dplyr::bind_rows(bag)
  bag %>%
    offset(cells, direction,
           n = abs(near_boundary - 
                   rowcol_function_opposite(bag[[rowcol_text]])) - 1 + include)
}
