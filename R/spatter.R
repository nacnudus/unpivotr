#' Spread key-value pairs of mixed types across multiple columns
#'
#' @description
#' [spatter()] is like [tidyr::spread()] but for when different columns have
#' different data types.  It works on data that has come via [tidy_table()] or
#' [tidyxl::xlsx_cells()], where each row represents one cell of a table, and
#' the value of the cell is represented in a different column, depending on the
#' data type.
#'
#' @param .data A data frame where each row represents a cell, with columns
#'   `row` and `col`, usually a column `data_type`, and additional columns of
#'   cell values.
#' @param key The name of the column whose values will become column names
#' @param ... functions for formatting particular data types, named by the data
#'   type (the name of the column of `.data` that contains the cell value.
#' @param values Optional. The column of `.data` to use as the value of each
#'   cell.  Given as a bare variable name.  If omitted (the default), the `type`
#'   argument will be used instead.
#' @param types Optional. The column that names, for each row of `.data`, which
#'   column contains the cell value.  Defaults to `data_type`.
#' @export
#' @examples
#' # A tidy representation of cells of mixed data types
#' x <- data.frame(stringsAsFactors = FALSE,
#'         row = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
#'         col = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
#'   data_type = c("character", "character", "character", "numeric", "character",
#'                 "numeric", "character", "numeric"),
#'   character = c("Name", "Age", "Matilda", NA, "Nicholas", NA, "Olivia", NA),
#'     numeric = c(NA, NA, NA, 1, NA, 3, NA, 5))
#' x
#'
#' # How it would look in a spreadsheet
#' rectify(x)
#'
#' # How it looks after treating the cells in row 1 as headers
#' y <- behead(x, N, header)
#' y$col <- NULL # Drop the 'col' column
#' y
#'
#' # At this point you might want to do tidyr::spread(), but it won't work because
#' # you want to use both the `character` and `numeric` columns as the values.
#' tidyr::spread(y, header, numeric)
#' tidyr::spread(y, header, character)
#' spatter(y, header)
#'
#' # The difference between spatter() and tidyr::spread() is that spatter()
#' # needs to know which data-type to use for each cell beneath the headers.  By
#' # default, it looks at the `data_type` column to decide, but you can change
#' # that with the `types` argument.
#' y %>%
#'   dplyr::select(-data_type, -numeric) %>%
#'   dplyr::mutate(data_type_2 = "character") %>%
#'   spatter(header, types = data_type_2)
#'
#' # Alternatively you can name one specific column to use for the cell values.
#' y %>%
#'   dplyr::mutate(foo = letters[1:6]) %>%
#'   dplyr::select(header, row, foo) %>%
#'   spatter(header, values = foo)
#'
#' # The column used for the values is consumed before the spread occurs. If
#' # it's necessary for demarking the rows, then make a copy of it first,
#' # otherwise you'll get an error like "Duplicate identifiers for rows ..."
#' y %>%
#'   dplyr::mutate(row2 = row) %>%
#'   dplyr::select(row, header, row2) %>%
#'   spatter(header, values = row2)
#'
#' # Like tidyr::spread(), you need to discard extraneous columns beforehand.
#' # Otherwise you can get more rows out than you want.
#' y$extra <- 11:16
#' spatter(y, header)
#'
#' # pack() is an easy way to keep just the columns you need, without knowing
#' # in advance which data-type columns you need.  This examples adds a new
#' # column, which is then removed by the pack-unpack sequence without having to
#' # mention it by name.
#' x$extra <- 11:18
#' x %>%
#'   pack() %>%
#'   dplyr::select(row, col, value) %>%
#'   unpack()
#'
#' # spatter() automatically converts data types so that they can coexist in the
#' # same column.  Ordered factors in particular will always be coerced to
#' # unordered factors.
#'
#' # You can control data type conversion by supplying custom functions, named
#' # by the data type of the cells they are to convert (look at the `data_type`
#' # column).  If your custom functions aren't sufficient to avoid the need for
#' # coercion, then they will be overridden.
#' spatter(y, header, character = ~ toupper(.), numeric = as.complex)
spatter <- function(.data, key, ..., values = NULL, types = data_type) {
  UseMethod("spatter")
}

#' @export
spatter.data.frame <- function(.data, key, ..., values = NULL,
                               types = data_type) {
  key <- rlang::ensym(key)
  dots <- list(...)
  functions <- purrr::map(dots, purrr::as_mapper)
  values <- rlang::enexpr(values)
  new_colnames <- format(unique(dplyr::pull(.data, !! key)), justify = "none")
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
  first_colnames <- setdiff(colnames(out), new_colnames)
  last_colnames <- sort(new_colnames)
  out <- dplyr::select(out, first_colnames, last_colnames)
  out
}
