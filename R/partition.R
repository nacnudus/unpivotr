#' Divide a grid of cells into partitions containing individual tables
#'
#' @description
#' Given the positions of corner cells that mark individual tables in a single
#' spreadsheet, `partion()` works out which table cells belong to which corner
#' cells.  The individual tables can then be worked on independently.
#'
#' `partition()` partitions along both dimensions (rows and columns) at once.
#' `partition_dim()` partitions along one dimension at a time.
#'
#' @param cells Data frame or tbl, the cells to be partitioned, from
#'   [as_cells()] or [tidyxl::xlsx_cells()].
#' @param corners usually a subset of `cells`, being the corners of individual
#'   tables.  Can also be cells that aren't among `cells`, in which case see the
#'   `strict` argument.
#' @param align Character, the position of the corner cells relative to their
#'   tables, one of `"top-left"` (default), `"top-right"`, `"bottom-left"`,
#'   `"bottom-right"`.
#' @param nest Logical, whether to nest the partitions in a list-column of data
#'   frames.
#' @param strict Logical, whether to omit partitions that don't contain a corner
#'   cell.
#' @param positions Integer vector, the positions of cells (either the row
#'   position or the column position), which are to be grouped between
#'   cutpoints.
#' @param cutpoints Integer vector. The `positions` will be separated into
#'   groups either side of each cutpoint.
#' @param bound One of `"upper"` or `"lower"`, controls whether cells that lie
#'   on a cutpoint are should be grouped with cells below or above the cutpoint.
#'   For example, if column 5 is a cutpoint, and a cell is in column 5,
#'   `"lower"` would group it with cells in columns 1 to 4, whereas `"upper"`
#'   would group it with cells in columns 6 to 10.  This is so that you can use
#'   cells at the bottom or the right-hand side of a table as the cutpoints
#'   (either of which would be 'upper' bounds because row and column numbers
#'   count from 1 in the top-left row and column).  When `"upper"`, any
#'   `cell_positions` above the first cutpoint will be in group 0; when
#'   `"lower"`, any `cell_positions` below the final cutpoint will be 0.
#'
#' @return `partition_dim()` returns an integer vector, numbering the groups of
#'   cells.  Group 0 represents the cells above the first cutpoint (when `bound
#'   = "upper"`), or below the first cutpoint (when `bound = "lower"`).  The
#'   other groups are numbered from 1, where group 1 is adjacent to group 0.
#'
#' @export
#' @examples
#' # The `purpose` dataset, represented in four summary tables
#' multiples <- purpose$small_multiples
#' rectify(multiples, character, numeric)
#'
#' # The same thing in its raw 'melted' form that can be filtered
#' multiples
#'
#' # First, find the cells that mark a corner of each table
#' corners <-
#'   dplyr::filter(multiples,
#'                 !is.na(character),
#'                 !(character %in% c("Sex", "Value", "Female", "Male")))
#'
#' # Then find out which cells fall into which partition
#' partition(multiples, corners)
#'
#' # You can also use bottom-left corners (or top-right or bottom-right)
#' bl_corners <- dplyr::filter(multiples, character == "Male")
#' partition(multiples, bl_corners, align = "bottom_left")
#'
#' # To complete the grid even when not all corners are supplied, use `strict`
#' bl_corners <- bl_corners[-1, ]
#' partition(multiples, bl_corners, align = "bottom_left")
#' partition(multiples, bl_corners, align = "bottom_left", strict = FALSE)
partition <- function(cells, corners, align = "top_left",
                      nest = TRUE, strict = TRUE) {
  check_distinct(cells)
  if (!(align %in% c(
    "top_left", "top_right",
    "bottom_left", "bottom_right"
  ))) {
    stop("`align` must be one of:
         \"top_left\", \"top_right\", \"bottom_left\", \"bottom_right\"")
  }
  row_bound <- (if (align %in% c("top_left", "top_right")) "upper" else "lower")
  col_bound <- (if (align %in% c("top_left", "bottom_left")) "upper" else "lower")
  row_groups <- partition_dim(cells$row, sort(unique(corners$row)), row_bound)
  col_groups <- partition_dim(cells$col, sort(unique(corners$col)), col_bound)
  join_names <- c("row", "col")
  names(join_names) <- c("corner_row", "corner_col")
  out <-
    cells %>%
    dplyr::mutate(
      corner_row = row_groups,
      corner_col = col_groups
    ) %>%
    tidyr::nest(cells = -tidyselect::one_of("corner_row", "corner_col")) %>%
    dplyr::inner_join(corners, by = join_names)
  if (strict) {
    out <- dplyr::filter(out, purrr::map_lgl(cells, contains_corner, corners))
  }
  if (!nest) {
    out <- tidyr::unnest(out, cells)
  }
  out
}

contains_corner <- function(.data, corners) {
  nrow(dplyr::inner_join(.data, corners, by = c("row", "col"))) != 0L
}

#' @describeIn partition Divide a grid of cells into chunks along one dimension
#'
#' @return
#' `partition_dim()` returns an integer vector, numbering the groups of cells.
#' Group 0 represents the cells above the first cutpoint (when `bound =
#' "upper"`), or below the first cutpoint (when `bound = "lower"`).  The other
#' groups are numbered from 1, where group 1 is adjacent to group 0.  Divide a
#' grid of cells into chunks along both dimensions
#'
#' @export
#' @examples
#' # Given a set of cells in rows 1 to 10, partition them at the 3rd, 5th and 7th
#' # rows.
#' partition_dim(1:10, c(3, 5, 7))
#'
#' # Given a set of cells in columns 1 to 10, partition them at the 3rd, 5th and
#' # 7th column.  This example is exactly the same as the previous one, to show
#' # that the function works the same way on columns as rows.
#' partition_dim(1:10, c(3, 5, 7))
#'
#' # Given a set of cells in rows 1 to 10, partition them at the 3rd, 5th and
#' # 7th rows, aligned to the bottom of the group.
#' partition_dim(1:10, c(3, 5, 7), bound = "lower")
#'
#' # Non-integer row/column numbers and cutpoints can be used, even though they
#' # make no sense in the context of partioning grids of cells.  They are
#' # rounded towards zero first.
#' partition_dim(1:10 - .5, c(3, 5, 7))
#' partition_dim(1:10, c(3, 5, 7) + 1.5)
partition_dim <- function(positions, cutpoints, bound = "upper") {
  if (!(bound %in% c("upper", "lower"))) {
    stop("`bound` must be one of \"upper\", \"lower\"")
  }
  cutpoints <- sort(c(-Inf, cutpoints, Inf), decreasing = bound == "lower")
  labels <- trunc(sort(cutpoints[-length(cutpoints)]))
  labels[is.infinite(labels)] <- NA
  out <- cut(positions, cutpoints, right = bound == "lower", labels = FALSE)
  labels[out]
}

# Other names that were considered for 'partition()'
# * grid
# * griddify
# * chop
# * chunk
# * dice
# * section
# * dissect
# * unfacet
# * find_facet
# * facet_id
# * set_facet
# * facet_set
# * facets_set
# * unmultiple
# * divide
# * divisi
# * conquer
