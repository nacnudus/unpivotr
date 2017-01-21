#' Extend a bag of cells in one direction, optionally up to a boundary
#' condition.
#'
#' @description A bag of data cells is a data frame with at least the
#' columns 'row' and 'col', as well as any others that carry information about
#' the cells, e.g. their values.  More cells may be added to the bag by
#' extending the bag in a given direction, either by a number of rows or
#' columns, or up to (and optionally including) cells that meet a boundary
#' condition.  The boundary may be required to be detected in every cell along
#' the 'leading edge' of the bag, otherwise extension will stop at the nearest
#' boundary that is detected.
#' @param bag Data frame. The original selection, including at least the columns
#' 'row' and 'column', which are numeric/integer vectors.
#' @param cells Data frame. All the cells in the sheet, among which to extend
#' the bag (extensions beyond existing cells will be padded with blank cells).
#' Must include at least the columns 'row' and 'column', as well as any columns
#' referred to by the boundary formula.
#' @param direction Character vector length 1. The direction in which to extend,
#' among the compass directions "N", "E", "S", "W", where "N" is north (up).
#' @param n Integer vector length 1, >= 0. The number of rows/cols to extend by
#' in the given direction.
#' @param boundary Formula to express a boundary condition.  `~ col <= 50` would
#' go up to the 50th column.  NAs are treated the same as FALSE, but with a
#' warning.
#' @param include Logical vector length 1. Whether to include in the extension
#' the first cell (and its row/col of fellow cells) at which the boundary
#' condition is met.
#' @param edge Logical vector length 1. Whether to require the boundary formula
#' to be TRUE along the entire leading edge of the bag that is being extended.
#' @details A bag may have ragged rows or ragged cols. Gaps will be filled in,
#' even when n = 0.
#' @name extend
#' @export
#' @examples
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#' # Make a tidy representation
#' cells <- tidytable(x, rownames = FALSE, colnames = FALSE)
#' cells <- cells[!is.na(cells$character), ] # Introduce 'holes' in the data
#' # Select a particular cell
#' cell <- cells[which(cells$row == 3 & cells$col == 3), ]
#' # Extend the selection downwards, stopping before the NA.
#' extend_S(cell, cells, boundary = ~ is.na(character))
#' # Extend the selection right, up to and including the fifth column.
#' extend_E(cell, cells, boundary = ~ col == 5, include = TRUE)
#' # Extend the selection beyond the existing cells
#' extend_E(cell, cells, 15)
#' # This doesn't work inside formulas, because it would mean testing the
#' # boundary formula on every possible cell in the given direction
#' \dontrun{extend_E(cell, cells, boundary = ~ col == 15)}
#' cell <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
#' extend_N(cell, cells, boundary = ~ !is.na(character), edge = TRUE)
extend <- function(bag, cells, direction, n = NULL, boundary = NULL,
                   edge = FALSE, include = FALSE) {
  test_extend_args(bag, direction, n, boundary, edge, include)
  if (!is.null(n)) {
    if (n == 0) {return(bag)}
    n <- as.integer(n) # Prevents coercion of row/col to double
    extend_n(bag, cells, direction, n)
  } else {
    extend_boundary(bag, cells, direction, boundary, edge, include)
  }
}

#' @describeIn extend Extend a bag of cells to the north
#' @export
extend_N <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  extend(bag, cells, "N", n, boundary, edge, include)
}

#' @describeIn extend Extend a bag of cells to the east
#' @export
extend_E <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  extend(bag, cells, "E", n, boundary, edge, include)
}

#' @describeIn extend Extend a bag of cells to the south
#' @export
extend_S <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  extend(bag, cells, "S", n, boundary, edge, include)
}

#' @describeIn extend Extend a bag of cells to the west
#' @export
extend_W <- function(bag, cells, n = NULL, boundary = NULL, edge = FALSE,
                     include = FALSE) {
  extend(bag, cells, "W", n, boundary, edge, include)
}

test_extend_args <- function(bag, direction, n, boundary, edge, include) {
  if (nrow(bag) == 0) {
    stop("Cannot extend an empty bag ('bag' has no rows)")
  }
  # Tests arguments to `extend_` functions.
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

extend_n <- function(bag, cells, direction, n) {
  n <- as.integer(n)
  if (direction == "N") {
    y1 <- max(min(bag$row) - 1L, 1L)
    y2 <- y1 - n + 1L
    x1 <- min(bag$col)
    x2 <- max(bag$col)
  }
  if (direction == "E") {
    y1 <- min(bag$row)
    y2 <- max(bag$row)
    x1 <- max(bag$col) + 1L
    x2 <- x1 + n - 1L
  }
  if (direction == "S") {
    y1 <- max(bag$row) + 1L
    y2 <- y1 + n - 1L
    x1 <- min(bag$col)
    x2 <- max(bag$col)
  }
  if (direction == "W") {
    y1 <- min(bag$row)
    y2 <- max(bag$row)
    x1 <- max(min(bag$col) - 1L, 1L)
    x2 <- x1 - n + 1L
  }
  cells %>%
    dplyr::filter(row >= y1,
           row <= y2,
           col >= x1,
           col <= x2) %>%
    pad(c(y1, y2), c(x1, x2)) %>%
    dplyr::bind_rows(bag) %>%
    dplyr::distinct()
}

extend_boundary <- function(bag, cells, direction, boundary, edge, include) {
  if (direction == "N") {
    rowcol_function <- max
    rowcol_function_opposite <- min
    rowcol_formula <- ~ row
    lt_gt <- `>`
    y1 <- max(min(cells$row) - 1L, 1L)
    y2 <- min(bag$row) - 1L
    x1 <- min(bag$col)
    x2 <- max(bag$col)
    include <- 0 - include
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
    include <- 0 - include
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
  cells %>%
    dplyr::filter_(lazyeval::interp(~ lt_gt(rowcol, near_boundary + include),
                                    rowcol = rowcol_name)) %>%
    dplyr::select(-.boundary) %>%
    dplyr::bind_rows(bag)
}
