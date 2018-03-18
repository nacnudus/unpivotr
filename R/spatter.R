#' Spatter a key-values set across multiple columns.
#'
#' @param data A data frame.
#' @export
#' @examples
#' path <- system.file("extdata", "worked-examples.xlsx", package = "unpivotr")
#' cells <- tidyxl::xlsx_cells(path, sheet = "clean")
#' rectify(cells)
#'
#' # This is what you start with: rows describing each data cell alongside its
#' # headers
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, data_type, header, character, numeric)
#'
#' # spatter() turns a column of headers into an actual header, a bit like
#' # tidyr::spread()
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, data_type, header, character, numeric) %>%
#'   spatter(header)
#'
#' # The difference between spatter() and tidyr::spread() is that spatter() has
#' # to know which data-type to use for each cell beneath the headers.  By
#' # default, it looks at the data_type column to decide, but you can choose a
#' # different column.
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::mutate(data_type_2 = "character") %>%
#'   dplyr::select(row, data_type_2, header, character) %>%
#'   spatter(header, types = data_type_2)
#'
#' # You can instead name one specific column to use for the cell values. In
#' # this case we use the `address` column, which also help to show what
#' # spatter() does.
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, header, address) %>%
#'   spatter(header, address)
#'
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, col, header) %>%
#'   spatter(header, col)
#'
#' # The column used for the values is dropped, so if it's necessary for
#' # demarking the rows, you'll need to create a copy of it first.  Otherwise
#' # you'll get an error like "Duplicate identifiers for rows ..."
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::mutate(row2 = row) %>%
#'   dplyr::select(row, header, row2) %>%
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
    types <- rlang::sym("data_type")
    .data <-
      .data %>%
      dplyr::rename(value = !! values) %>%
      dplyr::mutate(!! types := "value")
    factors <- character()
    if(is.list(.data$value)) factors <- "value"
  }
  if(is.null(values)) {
    drop_types <- rlang::expr_text(key) != rlang::expr_text(types)
  } else {
    drop_types <- rlang::expr_text(key) != "data_type"
  }
  .data %>%
    pack(types = !! types, name = ".value", drop_types = drop_types) %>%
    tidyr::spread(!! key, .value) %>%
    dplyr::mutate_if(is.list, concatenate) %>%
    # 2nd pass because factors are doubly listed
    dplyr::mutate_at(factors, concatenate)
}
