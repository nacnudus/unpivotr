#' Flood a header over its domain of data cells
#'
#' @description [flood()] takes a collection of cells with row and column
#' positions, ones that are headers marked with a direction of influence, and
#' associates the remaining cells with the header cells that govern them.
#' It only works on tables that have first come through [as_cells()] or
#' [tidyxl::xlsx_cells()].
#'
#' @param cells Data frame. The cells of a pivot table, usually the output of
#'   [as_cells()] or [tidyxl::xlsx_cells()], or of a subsequent operation on
#'   those outputs, with an additional column `direction` marking the header
#'   cells and the direction of their domain.
#' @param direction A bare variable name of the column that marks header cells
#'   and the direction of their domain.  Default: `direction`. See `?direction`
#'   for the options.
#' @param name A name to give the new column that will be created. Default:
#'   `"header"`.
#'
#' @return A data frame
#'
#' @export
#' @examples
#' # A simple table with a row of headers
#' (x <- data.frame(a = 1:2, b = 3:4))
#'
#' # Make a tidy representation of each cell
#' (cells <- as_cells(x, col_names = TRUE))
#'
#' # Strip the cells in row 1 (the original headers) and use them as data
#' behead(cells, "N", foo)
#'
#' # More complex example: pivot table with several layers of headers
#' (x <- purpose$`up-left left-up`)
#'
#' # Make a tidy representation
#' cells <- as_cells(x)
#' head(cells)
#' tail(cells)
#'
#' # Strip the headers and make them into data
#' tidy <-
#'   cells %>%
#'   behead("up-left", Sex) %>%
#'   behead("up", `Sense of purpose`) %>%
#'   behead("left-up", `Highest qualification`) %>%
#'   behead("left", `Age group (Life-stages)`) %>%
#'   dplyr::mutate(count = as.integer(chr)) %>%
#'   dplyr::select(-row, -col, -data_type, -chr)
#' head(tidy)
#'
#' # Check against the provided 'tidy' version of the data.
#' dplyr::anti_join(tidy, purpose$Tidy)
#'
#' # The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
#' # postgraduate qualification and a sense of purpose between 0 and 6.  That
#' # seems to have been an oversight by Statistics New Zealand.
#'
#' cells <- tibble::tribble(
#'        ~X1, ~adult, ~juvenile,
#'     "LION",    855,       677,
#'     "male",    496,       322,
#'   "female",    359,       355,
#'    "TIGER",    690,       324,
#'     "male",    381,       222,
#'   "female",    309,       102
#'   )
#' cells <- as_cells(cells, col_names = TRUE)
#'
#' cells %>%
#'   behead_if(chr == toupper(chr), direction = "left-up", name = "species") %>%
#'   behead("left", "sex") %>%
#'   behead("up", "age") %>%
#'   dplyr::select(species, sex, age, population = dbl)
flood <- function(cells, direction = direction, name = "header") {
  UseMethod("flood")
}

#' @export
flood.grouped_df <- function(cells, direction = direction, name = "header") {
  # Remember the grouping variables
  grouping_variables <- dplyr::group_vars(cells)
  cells %>%
    dplyr::group_split() %>%
    purrr::map_dfr(
      flood,
      direction = !!rlang::ensym(direction),
      name = !!rlang::ensym(name)
    ) %>%
    # Restore the grouping variables
    dplyr::group_by(!!!rlang::syms(grouping_variables))
}

check_direction_flood <- function(direction) {
  direction %in% c(
    "up", "up-left", "up-right",
    "down", "down-left", "down-right",
    "right", "right-up", "right-down",
    "left", "left-up", "left-down",
    "up-ish", "down-ish", "left-ish", "right-ish"
  )
}

#' @export
flood.data.frame <- function(cells, direction = direction, name = "header") {
  check_direction_flood(dplyr::pull(cells, {{ direction }}))
  check_distinct(cells)
  name <- rlang::ensym(name)
}

# # Which header owns the cell in the middle of the cross?
# | | |x| |
# | | |1| |
# |y|1|?|3|
# | | |3| |

# # How could nested headers be handled?
# | | |x| |d|e|
# |a|0|y|z|0|0|
# | |b|1|2|1|1|
# | |c|3|4|2|2|

list(
  a = "right",
  list(
    # The domain of x is a barrier to a
    x = "down-right",
    list(
      # The domains of y, z, a, b are not barriers to x
      y,z = "down",
      b,c = "right",
    )
  ),
  # The domain of d is a barrier to x
  d,e = "down",
)

# Is this equivalent?
list(
  a = "right",
  list(
    # The domain of x is a barrier to a
    b,c = "right",
    x = "down-right",
    list(
      # The domains of y, z, a, b are not barriers to x
      y,z = "down",
    )
  ),
  # The domain of d is a barrier to x
  d,e = "down",
)

filter_direction <- function(cells, header, direction) {
  directions <- strsplit(direction, "-", fixed = TRUE)[[1]]
  # browser()
  cells <- switch(
      directions[1],
      up = dplyr::filter(cells, row <= header$row),
      right = dplyr::filter(cells, col >= header$col),
      down = dplyr::filter(cells, row >= header$row),
      left = dplyr::filter(cells, col <= header$row)
  )
  if (length(directions) == 1) {
    cells <- switch(
        directions[1],
        up = dplyr::filter(cells, col == header$col),
        right = dplyr::filter(cells, row == header$row),
        down = dplyr::filter(cells, col == header$col),
        left = dplyr::filter(cells, row == header$row)
    )
  } else {
    cells <- switch(
        directions[2],
        up = dplyr::filter(cells, row <= header$row),
        right = dplyr::filter(cells, col >= header$col),
        down = dplyr::filter(cells, row >= header$row),
        left = dplyr::filter(cells, col <= header$row)
    )
  }
  cells
  dplyr::distinct(cells, row, col) |> dplyr::arrange(row, col)
}
# cells <- tidyxl::xlsx_cells("./inst/extdata/harry-potter.xlsx", sheet = 1)
# cells <- filter(cells, row <= 3, col <= 3)
# header <- filter(cells, row == 2, col == 2)
# filter_direction(cells, header, "up")
# filter_direction(cells, header, "up-left")
# filter_direction(cells, header, "up-right")
# filter_direction(cells, header, "right-up")
# filter_direction(cells, header, "right")
# filter_direction(cells, header, "right-down")
# filter_direction(cells, header, "down-right")
# filter_direction(cells, header, "down")
# filter_direction(cells, header, "down-left")
# filter_direction(cells, header, "left-down")
# filter_direction(cells, header, "left-")
# filter_direction(cells, header, "left-up")

purrr::map(directions, strsplit, split = "-", fixed = TRUE)
do.call(strsplit, list(directions, split = "-", fixed = TRUE))
strsplit(direction, "-", fixed = TRUE)

# These are sets of trees.  Each table is a tree.  Just because they're
# cheek-by-jowl doesn't mean they aren't separate tables.  The two trees above
# are equivalent, because the leaves have the same ancestors, even though the
# ancestors aren't in the same order.
#
# Or rather, not trees, because each leaf can have two parents, but that's the
# limit, and a leaf has no children.  Defining a "leaf" in a directed graph as
# being a node with no "out" edges.
#
# 1. Parse cells
# 2. Take the first header
# 3. Filter for cells in that header's domain.
# 4. Assign those cells as direct children of the header.
# 5. Take the next header at the same level.
# 6. Filter for cells in that header's domain.
# 7. Assign those cells as direct children of the header.  Any cells that had
#    already been assigned to the first header will be reassigned to the second.
# 8. When the first level is complete, take the first header of the next level.
# 9. Filter for cells in that header's parent's domain.
# 10. Of those cells, filter for cells in that header's domain.
# 11. Assign those cells as direct children of the header.  Any cells that had
#    already been assigned to the first header will be reassigned to the second.
#
# Conflicts when two tables in the same sheet have opposing directions.
# Document and and advise breaking up with corners()?
#
# Also isn't commutative.  The order of siblings matters.
