#' Strip a level of headers from a pivot table
#'
#' @description [behead()] takes one level of headers from a pivot table and
#' makes it part of the data.  Think of it like [tidyr::gather()], except that
#' it works when there is more than one row of headers (or more than one column
#' of row-headers), and it only works on tables that have first come through
#' [tidy_table()] or [tidyxl::xlsx_cells()].
#'
#' @param cells Data frame. The cells of a pivot table, usually the output of
#' [tidy_table()] or [tidyxl::xlsx_cells()], or of a subsequent operation on
#' those outputs.
#' @param direction The name of a function that joins headers to data cells, one
#' of `N`, `E`, `S`, `W`, `NNW`, `NNE`, `ENE`, `ESE`, `SSE`, `SSW`. `WSW`,
#' `WNW`.  `ABOVE`, `BELOW`, `LEFT` and `RIGHT` aren't available because they
#' require certain ambiguities that are better handled by using the functions
#' directly rather than via [behead()].  See the help file for [join_header()].
#' @param name A name to give the new column that will be created, e.g.
#' `"location"` if the headers are locations.  Quoted (`"location"`, not
#' `location`) because it doesn't refer to an actual object.
#' @param types The name of the column that names the data type of each cell.
#' Usually called `data_types` (the default), this is a character column that
#' names the other columns in `cells` that contain the values of each cell.
#' E.g.  a cell with a character value will have `"character"` in this column.
#' Unquoted(`data_types`, not `"data_types"`) because it refers to an actual
#' object.
#' @param drop_na logical Whether to filter out headers that have `NA` in the
#' `value` column.  Default: `TRUE`.  This can happen with the output of
#' `tidyxl::xlsx_cells()`, when an empty cell exists because it has formatting
#' applied to it, but should be ignored.
#'
#' @return A data frame
#'
#' @export
#' @examples
#' # Load some pivoted data
#' (x <- purpose$`NNW WNW`)
#'
#' # Make a tidy representation
#' cells <- tidy_table(x)
#' head(cells)
#' tail(cells)
#'
#' # Strip the headers and make them into data
#' tidy <-
#'   cells %>%
#'   behead(NNW, Sex) %>%
#'   behead(N, `Sense of purpose`) %>%
#'   behead(WNW, `Highest qualification`) %>%
#'   behead(W, `Age group (Life-stages)`) %>%
#'   dplyr::select(-row, -col, -data_type, -chr)
#' head(tidy)
#'
#' # Check against the provided 'tidy' version of the data.
#' dplyr::anti_join(tidy, purpose$Tidy)
#'
#' # The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
#' # postgraduate qualification and a sense of purpose between 0 and 6.  That
#' # seems to have been an oversight by Statistics New Zealand.
behead <- function(cells, direction, name, types = data_type, drop_na = TRUE) {
  name <- rlang::ensym(name)
  types <- rlang::ensym(types)
  type_names <- unique(dplyr::pull(cells, !! types))
  filter_expr <- direction_filter(!!rlang::ensym(direction))
  is_header <- rlang::eval_tidy(filter_expr, cells)
  headers <-
    cells %>%
    dplyr::filter(is_header) %>%
    pack(types = !! types) %>%
    dplyr::mutate(is_na = is.na(value),
                  !! name := purrr::map_chr(value, format_list_element),
                  !! name := dplyr::if_else(is_na,
                                            NA_character_,
                                            !! name)) %>%
    dplyr::filter(!(drop_na & is_na)) %>%
    dplyr::select(row, col, !! name)
  datacells <- dplyr::filter(cells, !is_header)
  direction(datacells, headers, drop = FALSE)
}

# Apply format() to list-elements of a list-column created by pack(), descending
# into factors (which are doubly wrapped in lists)
format_list_element <- function(x) {
  if(is.list(x)) return(format(x[[1]]))
  format(x)
}

# Construct a filter expression for stripping a header from a pivot table
direction_filter <- function(direction) {
  direction <- rlang::expr_text(rlang::ensym(direction))
  check_direction(direction)
  direction <- substr(direction, 1L, 1L)
  dplyr::case_when(direction == "N" ~ rlang::expr(.data$row == min(.data$row)),
                   direction == "E" ~ rlang::expr(.data$col == max(.data$col)),
                   direction == "S" ~ rlang::expr(.data$row == max(.data$row)),
                   direction == "W" ~ rlang::expr(.data$col == min(.data$col)))
}

# Check that a given direction is a supported compass direction
check_direction <- function(direction_string) {
  directions <- c("NNW", "N", "NNE",
                        "ENE", "E", "ESE",
                        "SSE", "S", "SSW",
                        "WSW", "W", "WNW")
  if (!(direction_string %in% directions)) {
    stop("`direction` must be one of \"",
         paste(directions, collapse = "\", \""),
         "\"")
  }
}
