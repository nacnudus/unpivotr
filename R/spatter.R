#' Spatter a key-values set across multiple columns.
#'
#' @param data A data frame.
#' @export
#' @examples
#' path <- system.file("extdata", "worked-examples.xlsx", package = "unpivotr")
#' cells <- tidyxl::xlsx_cells(path, sheet = "clean")
#' rectify(cells)
#'
#' # This is what you start with: rows describing each data cell and its header
#' xlsx_cells(path, sheet = "clean") %>%
#'   behead(N, header) %>%
#'   select(row, data_type, header, character, numeric)
#'
#' # You want the header column to become an actual header, so you spatter() it
#' xlsx_cells(path, sheet = "clean") %>%
#'   behead(N, header) %>%
#'   select(row, data_type, header, character, numeric) %>%
#'   spatter(header)
#'
#' # spatter() chooses the cell values by looking at the data_type column by
#' # default, but you can choose a different column of types to look at.
#' cells %>%
#'   behead(N, header) %>%
#'   mutate(data_type_2 = "character") %>%
#'   select(row, data_type_2, header, character) %>%
#'   spatter(header, types = data_type_2)
#'
#' # You can also name one specific column to use for the cell values. In this
#' # case we use the `address` column, which helps show what spatter() does.
#' cells %>%
#'   behead(N, header) %>%
#'   select(row, header, address) %>%
#'   spatter(header, address)
#'
#' cells %>%
#'   behead(N, header) %>%
#'   select(row, col, header) %>%
#'   spatter(header, col)
#'
#' # The column used for the values is dropped, so if it's necessary for
#' demarking the rows, you'll need to create a copy of it first.  Otherwise
#' you'll get an error like "Duplicate identifiers for rows ..."
#' cells %>%
#'   behead(N, header) %>%
#'   mutate(row2 = row) %>%
#'   select(row, header, row2) %>%
#'   spatter(header, row2)
spatter <- function(.data, key, values, types = data_type) {
  UseMethod("spatter")
}

#' @export
spatter.data.frame <- function(.data, key, values = NULL, types = data_type) {
  key <- rlang::ensym(key)
  values <- rlang::enexpr(values)
  if(is.null(values)) {
    types <- rlang::ensym(types)
    factors <-
      .data %>%
      dplyr::distinct(!! key, !! types) %>%
      dplyr::filter(!! types %in% c("fct", "ord")) %>%
      dplyr::pull(!! key)
  } else {
    values <- rlang::ensym(values)
    colname <- rlang::expr_text(values)
    types <- rlang::sym("data_type")
    .data <-
      .data %>%
      dplyr::rename(value = !! values) %>%
      dplyr::mutate(!! types := "value")
    factors <- character()
    if(is.list(.data$value)) factors <- "value"
  }
  .data %>%
    pack(types = !! types, name = ".value") %>%
    tidyr::spread(!! key, .value) %>%
    dplyr::mutate_if(is.list, concatenate) %>%
    # 2nd pass because factors are doubly listed
    dplyr::mutate_at(factors, concatenate)
}
