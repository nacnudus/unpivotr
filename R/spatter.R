#' Spatter a key-values set across multiple columns.
#'
#' @param data A data frame.
#' @param values Optional. The column of `cells` to use as the values of each
#' cell.  Given as a bare variable name.  If omitted (the default), the `types`
#' argument will be used instead.
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
#' # The difference between spatter() and tidyr::spread() is that spatter()
#' # needs to know which data-type to use for each cell beneath the headers.  By
#' # default, it looks at the `data_type` column to decide, but you can change
#' # that with the `types` argument.
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::mutate(data_type_2 = "character") %>%
#'   dplyr::select(row, data_type_2, header, character) %>%
#'   spatter(header, types = data_type_2)
#'
#' # Alternatively you can name one specific column to use for the cell values.
#' # In this case we use the `address` column, which also helps to show what
#' # spatter() does.
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, header, address) %>%
#'   spatter(header, values = address)
#'
#' # The column used for the values is consumed before the spread occurs. If
#' # it's necessary for demarking the rows, then make a copy of it first,
#' # otherwise you'll get an error like "Duplicate identifiers for rows ..."
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::mutate(row2 = row) %>%
#'   dplyr::select(row, header, row2) %>%
#'   spatter(header, values = row2)
#'
#' # spatter() automatically converts data types so that they can coexist in the
#' # same column.  Ordered factors in particular will always be coerced to
#' # unordered factors.
#'
#' # You can control the conversion yourself by supplying custom functions,
#' # named by the data type of the cells they are to convert (look at the
#' # `data_type` column).  If your custom functions aren't sufficient to avoid
#' # the need for coercion, then they will be overridden.
#' cells %>%
#'   behead(N, header) %>%
#'   dplyr::select(row, data_type, header, character, numeric) %>%
#'   spatter(header, character = ~ toupper(.), numeric = as.complex)
spatter <- function(.data, key, ..., values, types = data_type) {
  UseMethod("spatter")
}

#' @export
spatter.data.frame <- function(.data, key, ..., values = NULL,
                               types = data_type) {
  key <- rlang::ensym(key)
  dots <- list(...)
  functions <- purrr::map(dots, purrr::as_mapper)
  values <- rlang::enexpr(values)
  if(is.null(values)) {
    types <- rlang::ensym(types)
    original_types <- NULL
  } else {
    original_types <- rlang::ensym(types)
    types <- rlang::sym(".data_type")
    .data <-
      .data %>%
      dplyr::mutate(.value = !! values) %>%
      dplyr::mutate(!! types := ".value")
    if(!(rlang::expr_text(values) %in% c(rlang::expr_text(key)))) {
        .data <- dplyr::select(.data, - !! values)
    }
  }
  if(is.null(values)) {
    drop_types <- rlang::expr_text(types) != rlang::expr_text(key)
  } else {
    drop_types <- !(c(".data_type") %in% c(rlang::expr_text(key),
                                           rlang::expr_text(values)))
  }
  out <- pack(.data, types = !! types, name = ".value", drop_types = drop_types)
  # Calculate the positions of cols to be created by spread()
  n_keys <- length(unique(dplyr::pull(.data, !! key)))
  n_cols <- ncol(out) - 2 + n_keys
  new_col_positions <- seq_len(n_keys) + (n_cols - n_keys)
  out <-
    out %>%
    dplyr::mutate(.value = purrr::imap(.value,
                                       maybe_format_list_element,
                                       functions)) %>%
    tidyr::spread(!! key, .value) %>%
    dplyr::mutate_at(new_col_positions, concatenate)
  if(!is.null(original_types) &&
     rlang::expr_text(original_types) %in% colnames(out)) {
    out <- dplyr::select(out, - !! original_types)
  }
  out
}
