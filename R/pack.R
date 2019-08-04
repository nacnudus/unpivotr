#' Pack cell values from separate columns per data type into one list-column
#'
#' @details
#' When cells are represented by rows of a data frame, the values of the cells
#' will be in different columns according to their data type.  For example, the
#' value of a cell containing text will be in a column called `chr` (or
#' `character` if it came via tidyxl).  A column called `data_type` names, for
#' each cell, which column its value is in.
#'
#' [pack()] rearranges the cell values in a different way, so that they are all
#' in one column, by
#'
#' 1. taking each cell value, from whichever column.
#' 1. making it an element of a list.
#' 1. naming each element according to the column it came from.
#' 1. making the list into a new list-column of the original data frame.
#'
#' By default, the original columns are dropped, and so is the `data_type`
#' column.
#'
#' [unpack()] is the complement.
#'
#' This can be useful for dropping all columns of `cells` except the ones that
#' contain data.  For example, [tidyxl::xlsx_cells()] returns a very wide data
#' frame, and to make it narrow you might do:
#'
#' ```
#' select(cells, row, col, character, numeric, date)
#' ```
#'
#' But what if you don't know in advance that the data types you need are
#' `character`, `numeric` and `date`?  You might also need `logical` and
#' `error`.
#'
#' Instead, [pack()] all the data types into a single column, select it, and
#' then unpack.
#'
#' ```
#' pack(cells) %>%
#'   select(row, col, value) %>%
#'   unpack()
#' ```
#'
#' @param cells A data frame of cells, one row per cell.  For [pack()] it must
#' have a column that names, for each cell/row, which of the other columns the
#' value is in.  For [unpack()] it must have a list-column of cell values, where
#' each element is named according to the data type of the value.
#'
#' @param types For [pack()], the name of the column that that names, for each
#' cell/row, which of the other columns the value is in.
#'
#' @param name A string. For [pack()], the name to give the new list-column of
#' values.  For [unpack()], the name to give the new column that will name, for
#' each cell, which of the other columns the value is in.
#'
#' @param drop_types For [pack()], whether to drop the column named by `types`.
#'
#' @param drop_type_cols For [pack()], whether to drop the original columns of
#' cell values.
#'
#' @param values For [unpack()], the name of the list-column of cell values.
#'
#' @param drop_packed For [unpack()], whether to drop the column named by
#' `values`.
#'
#' @export
#' @examples
#' # A normal data frame
#' w <- data.frame(foo = 1:2,
#'                 bar = c("a", "b"),
#'                 stringsAsFactors = FALSE)
#' w
#'
#' # The same data, represented by one row per cell, with integer values in the
#' # `int` column and character values in the `chr` column.
#' x <- as_cells(w)
#' x
#'
#' # pack() and unpack() are complements
#' pack(x)
#' unpack(pack(x))
#'
#' # Drop non-data columns from a wide data frame of cells from tidyxl
#' if (require(tidyxl)) {
#'   cells <- tidyxl::xlsx_cells(system.file("extdata", "purpose.xlsx", package = "unpivotr"))
#'   cells
#'
#'   pack(cells) %>%
#'     dplyr::select(row, col, value) %>%
#'     unpack()
#' }
pack <- function(cells, types = data_type, name = "value", drop_types = TRUE,
                 drop_type_cols = TRUE) {
  types <- rlang::ensym(types)
  name <- rlang::ensym(name)
  type_colnames <- format(unique(dplyr::pull(cells, !!types)),
    justify = "none",
    trim = TRUE
  )
  # Default any types without corresponding value columns to NA
  missing_types <- setdiff(type_colnames, colnames(cells))
  new_cols <- rep_len(NA, length(missing_types))
  names(new_cols) <- missing_types
  cells <- dplyr::mutate(
    cells,
    !!!new_cols,
    !!types := format(!!types,
      justify = "none",
      trim = TRUE
    )
  )
  # Create the packed value column
  type_values <- unique(dplyr::pull(cells, !!types))
  patterns <-
    map(
      type_values,
      ~ rlang::expr(!!types == !!.x ~ as.list(!!rlang::ensym(.x)))
    )
  out <- dplyr::mutate(cells, !!name := dplyr::case_when(!!!patterns))
  # Name the elements of the packed value column by their types
  names(out[[rlang::expr_text(name)]]) <- dplyr::pull(cells, !!types)
  if (drop_types && rlang::expr_text(types) != rlang::expr_text(name)) {
    out <- dplyr::select(out, -!!types)
  }
  if (drop_type_cols) {
    type_colnames <- setdiff(type_colnames, rlang::expr_text(name))
    out <- dplyr::select(out, -dplyr::one_of(type_colnames))
  }
  out
}

#' @describeIn pack Unpack cell values from one list-column into separate columns per data type
#' @export
unpack <- function(cells, values = value, name = "data_type",
                   drop_packed = TRUE) {
  values <- rlang::ensym(values)
  name <- rlang::ensym(name)
  types <- names(dplyr::pull(cells, !!values))
  type_names <- format(unique(types), justify = "none", trim = TRUE)
  assignments <- purrr::map(
    type_names,
    ~ rlang::expr(ifelse(types == !!.x,
      !!values,
      !!list(NULL)
    ))
  )
  names(assignments) <- type_names
  out <-
    dplyr::mutate(cells, !!name := types, !!!assignments) %>%
    dplyr::mutate_at(type_names,
      concatenate,
      combine_factors = FALSE,
      fill_factor_na = FALSE
    )
  first_colnames <- setdiff(colnames(out), type_names)
  last_colnames <- sort(type_names)
  out <- dplyr::select(out, first_colnames, last_colnames)
  if (drop_packed) out <- dplyr::select(out, -!!values)
  out
}
