#' Divide a grid of cells into partitions containing individual tables
#'
#' @description Given the positions of corner cells that mark individual tables
#' in a single spreadsheet, `partion()` works out which cells belong to which
#' tables, and gives them an ID.  This can be used with `dplyr::group_by()` or
#' `tidyr::nest()` to operate on each single table within a spreadsheet of
#' several tables.
#'
#' Corners must all be in the top-left, or bottom-left, or top-right, or
#' bottom-right of each table.
#'
#' `partition()` partitions along both dimensions (rows and columns) at once.
#' `partition_dim()` partitions along one dimension at a time.
#'
#' @param cells Data frame or tbl, the cells to be partitioned, from
#' [tidy_table()] or [tidyxl::xlsx_cells()].
#' @param corners a subset of `cells`, being the corners of individual tables.
#' @param partition_name Character vector length 1, what to call the new column
#' that will be created to identify the partitions.  Default: `"partition"`.
#' @param positions Integer vector, the positions of cells (either the row
#' position or the column position), which are to be grouped between cutpoints.
#' @param cutpoints Integer vector. The `positions` will be separated into
#' groups either side of each cutpoint.
#' @param bound One of `"upper"` or `"lower"`, controls whether cells that lie
#' on a cutpoint are should be grouped with cells below or above the cutpoint.
#' For example, if column 5 is a cutpoint, and a cell is in column 5, `"lower"`
#' would group it with cells in columns 1 to 4, whereas `"upper"` would group it
#' with cells in columns 6 to 10.  This is so that you can use cells at the
#' bottom or the right-hand side of a table as the cutpoints (either of which
#' would be 'upper' bounds because row and column numbers count from 1 in the
#' top-left row and column).  When `"upper"`, any `cell_positions` above the
#' first cutpoint will be in group 0; when `"lower"`, any `cell_positions` below
#' the final cutpoint will be 0.
#'
#' @return `partition_dim()` returns an integer vector, numbering the groups of
#' cells.  Group 0 represents the cells above the first cutpoint (when `bound =
#' "upper"`), or below the first cutpoint (when `bound = "lower"`).  The other
#' groups are numbered from 1, where group 1 is adjacent to group 0.
#'
#' @export
#' @examples
#' # The `purpose` dataset, represented in four summary tables
#' multiples <- purpose$small_multiples
#' rectify(multiples, chr, num)
#'
#' # The same thing in its raw 'melted' form that can be filtered
#' multiples
#'
#' # First, find the cells that mark a corner of each table
#' corners <-
#'   dplyr::filter(multiples,
#'                 !is.na(chr),
#'                 !(chr %in% c("Sex", "Value", "Female", "Male")))
#'
#' # Then find out which cells fall into which partition
#' partition(multiples, corners)
#'
#' # It's more obvious if you also split() by the new 'partition' column
#' multiples <- multiples %>% arrange(col, row) %>% print(n = Inf)
#' x <- partition(multiples, corners)
#' split(x, x$partition)
#'
#' # An alternative to split() is tidyr::nest()
#' tidyr::nest(x, -partition)
#'
#' # You can also use bottom-left corners (or top-right or bottom-right)
#' bl_corners <- dplyr::filter(multiples, chr == "Yes")
#' y <- partition(multiples, bl_corners, position = "bottom_left")
#' split(y, y$partition)
#'
#' If `partition_name` is already the name of a column in `cells`, then it will
#' be silently overwritten
#' multiples$important_column <- "Will be overwritten"
#' partition(multiples, corners, partition_name = "important_column")
partition <- function(cells, corners, position = "top_left",
                     partition_name = "partition") {
  if (!(position %in% c("top_left", "top_right",
                       "bottom_left", "bottom_right"))) {
    stop("`position` must be one of:
         \"top_left\", \"top_right\", \"bottom_left\", \"bottom_right\"")
  }
  partition_name <- rlang::ensym(partition_name)
  row_bound <- (if(position %in% c("top_left", "top_right")) "upper" else "lower")
  col_bound <- (if(position %in% c("top_left", "bottom_left")) "upper" else "lower")
  row_groups <- partition_dim(cells$row, unique(corners$row), row_bound)
  col_groups <- partition_dim(cells$col, unique(corners$col), col_bound)
  cells %>%
    dplyr::mutate(!!partition_name := paste(row_groups, col_groups, sep = ",")) %>%
    dplyr::filter(row_groups >= 1, col_groups >= 1) %>%
    dplyr::mutate(!!partition_name := as.integer(factor(!!partition_name)))
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
#' # rows. Row 3 is the top row of group 1.
#' partition_dim(1:10, c(3, 5, 7))
#'
#' # Given a set of cells in columns 1 to 10, partition them at the 3rd, 5th and
#' # 7th column. Column 3 is the left-most column of group 1.
#' # This example is exactly the same as the previous one, to show that the
#' # function works the same way on columns as rows.
#' partition_dim(1:10, c(3, 5, 7))
#'
#' # Given a set of cells in rows 1 to 10, partition them at the 3rd, 5th and
#' # 7th rows. Row 7 is the bottom row of group 1
#' partition_dim(1:10, c(3, 5, 7), bound = "lower")
#'
#' # When the first row of cells is on the first cutpoint, there is no group 0.
#' partition_dim(1:10, c(1, 10))
#' partition_dim(1:10, c(1, 10), bound = "lower")
#'
#' # Non-integer row/column numbers and cutpoints can be used, even though they
#' # make no sense in the context of partioning grids of cells.
#' partition_dim(1:10 - .5, c(3, 5, 7))
#' partition_dim(1:10, c(3, 5, 7) + 1.5)
partition_dim <- function(positions, cutpoints, bound = "upper") {
  if (!(bound %in% c("upper", "lower"))) {
    stop("`bound` must be one of \"upper\", \"lower\"")
  }
  cutpoints <- c(-Inf, sort(cutpoints, decreasing = bound == "lower"), Inf)
  labels <-
    if(length(cutpoints) == 0) {
    } else {
      sort(seq_along(cutpoints[-1]) - 1, decreasing = bound == "lower")
    }
  cut(positions, cutpoints, right = bound == "lower", labels = labels) %>%
    as.character() %>%
    as.integer()
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
