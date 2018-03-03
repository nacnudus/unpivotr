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
#' `location` if the headers are locations.  Not quoted (not `"location"`).
#' @param values The name of the column of `cells` that contains the value of
#' the header cells.  E.g. `"chr"` if the headers are strings and the table came
#' via [tidy_table()].
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
#'   behead(NNW, Sex, chr) %>%
#'   behead(N, `Sense of purpose`, chr) %>%
#'   behead(WNW, `Highest qualification`, chr) %>%
#'   behead(W, `Age group (Life-stages)`, chr) %>%
#'   dplyr::select(-row, -col, -data_type, -chr)
#' head(tidy)
#'
#' # Check against the provided 'tidy' version of the data.
#' dplyr::anti_join(tidy, purpose$Tidy)
#'
#' # The provided 'tidy' data is missing a roww for Male 15-24-year-olds with a
#' # postgraduate qualification and a sense of purpose between 0 and 6.  That
#' # seems to have been an oversight by Statistics New Zealand.
behead <- function(cells, direction, name, values, drop_na = TRUE) {
  name <- rlang::ensym(name)
  values <- rlang::ensym(values)
  filter_expr <- direction_filter(!!rlang::ensym(direction))
  is_header <- rlang::eval_tidy(filter_expr, cells)
  headers <-
    cells %>%
    dplyr::filter(is_header) %>%
    dplyr::select(row, col, !!name := !!values)
  if (drop_na) headers <- dplyr::filter(headers, !is.na(!!name))
  datacells <- dplyr::filter(cells, !is_header)
  direction(datacells, headers, drop = FALSE)
}

# Construct a filter expression for stripping a header from a pivot table
direction_filter <- function(direction) {
  direction <- rlang::expr_text(rlang::ensym(direction))
  check_direction(direction)
  direction <- substr(direction, 1L, 1L)
  dplyr::case_when(direction == "N" ~ rlang::expr(row == min(row)),
                   direction == "E" ~ rlang::expr(col == max(col)),
                   direction == "S" ~ rlang::expr(row == max(row)),
                   direction == "W" ~ rlang::expr(col == min(col)))
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
