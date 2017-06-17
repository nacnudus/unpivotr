#' Convert data frames into a tidy structure
#'
#' @description Data frames represent data in a
#' tabular structure.  \code{tidy_table} takes the row and column position of
#' each 'cell', and returns that information in a new data frame, alongside the
#' content of each cell.
#'
#' This makes certain tasks easier.  For example, a pivot table with multi-row
#' headers that has been imported into R as a data frame may be easier to
#' un-pivot by converting it with \code{tidy_table} first.
#'
#' This is an S3 generic.
#'
#' @param x A data.frame
#' @param colnames Whether to include the row names in the output, Default:
#' FALSE
#' @param rownames Whether to include the column names in the output, Default:
#' FALSE
#'
#' @return A data.frame with columns 'row' and 'col' (integer) giving the
#' original position of the 'cells', and any relevant columns for cell values in
#' their original types: 'chr', 'cplx', 'cplx', 'dbl', 'fctr', 'int', 'lgl', and
#' 'list'.  Row and column names, when included (default), are treated as though
#' they were cells in the table.
#'
#' @examples
#' tidy_table(Formaldehyde)
#' tidy_table(tidyr::nest(chickwts, -feed))
#' tidy_table(Formaldehyde, colnames = TRUE)
#' tidy_table(Formaldehyde, rownames = TRUE)
#' @export
tidy_table <- function(x, rownames = FALSE, colnames = FALSE) {
  UseMethod("tidy_table")
}

# if_else is fussy about types, so return the correct type of NA
NA_type_ <- function(x) {
  switch(tibble::type_sum(x),
         NULL = NA,
         lgl = NA,
         int = NA_integer_,
         dbl = NA_real_,
         fctr = NA_integer_,
         chr = NA_character_,
         cplx = NA_complex_)
}

#' @export
tidy_table.data.frame <- function(x, rownames = FALSE, colnames = FALSE) {
  if (!rownames) {
    row.names(x) <- NULL
  }
  if (!colnames) {
    colnames(x) <- NULL
  }
  values <- purrr::flatten(lapply(x, as.list))
  nrows <- nrow(x)
  ncols <- ncol(x)
  types <- purrr::map_chr(x, tibble::type_sum)
  # Spread cells into different columns by data type
  out <- tibble::data_frame(row = rep.int(seq_len(nrow(x)), ncols),
                            col = rep(seq_len(ncol(x)), each = nrows),
                            value = values,
                            type = rep(types, each = nrows))
  out <- tidyr::spread(out, type, value)
  # Convert non-list-columns to vectors
  out <- dplyr::mutate_at(out,
                          dplyr::select_vars(names(out), dplyr::everything(),
                                             exclude = c("row",
                                                         "col",
                                                         "fctr",
                                                         "list")),
                          ~ unlist(purrr::map(.x, ~ dplyr::if_else(is.null(.x),
                                                                   NA_type_(.x),
                                                                   .x))))
  # Append row and column names
  if (rownames) {
    row_names <- row.names(x)
    out$col <- out$col + 1L
    out <- dplyr::bind_rows(out,
                            tibble::data_frame(col = 1L,
                                               row = seq_along(row_names),
                                               chr = row_names))
  }
  if (colnames) {
    col_names <- colnames(x)
    out$row <- out$row + 1L
    out <- dplyr::bind_rows(out,
                            tibble::data_frame(row = 1L,
                                               col = seq_along(col_names) + rownames,
                                               chr = col_names))
  }
  out <- dplyr::select(out, row, col, sort(colnames(out)))
  dplyr::arrange(out, col, row)
}
