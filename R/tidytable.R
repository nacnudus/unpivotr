#' Convert data frames into a tidy structure
#'
#' @description Data frames and matrices represent data in a tabular structure.
#' \code{tidytable} takes the row and column position of each 'cell', and
#' returns that information in a new data frame.  This makes certain tasks
#' easier.  For example, a pivot table with multi-row headers that has been
#' imported into R as a data frame may be easier to un-pivot by converting it
#' with \code{tidytable} first.
#'
#' This is an S3 generic.
#'
#' @param x A data.frame or a matrix
#' @param colnames Whether to include the row names in the output
#' @param rownames Whether to include the column names in the output
#'
#' @return A data.frame with six columns: 'row' and 'col' (integer) giving the
#' original position of the 'cells', and 'character', 'double', 'integer' and
#' 'logical' giving the cell values in their original types.  Row and column
#' names are returned in the 'character' column, and, when present, offset the
#' other cells by one row or column.
#'
#' @examples
#' tidytable(Formaldehyde)
#' tidytable(as.matrix(Formaldehyde))
#' tidytable(Formaldehyde, colnames = FALSE)
#' tidytable(Formaldehyde, rownames = FALSE)
#' @export
tidytable <- function(x, rownames = TRUE, colnames = TRUE) {
  UseMethod("tidytable")
}

blank_data_frame <- function(row = 1L, col = 1L, names) {
  out <- data.frame(row = row,
                    col = col,
                    character = names,
                    double = NA_real_,
                    integer = NA_integer_,
                    logical = NA,
                    stringsAsFactors = FALSE)
  out$list <- vector("list", max(length(row), length(col)))
  out
}

#' @export
tidytable.matrix <- function(x, rownames = TRUE, colnames = TRUE) {
  if (!rownames) {
    row.names(x) <- NULL
  }
  if (!colnames) {
    colnames(x) <- NULL
  }
  out <- expand.grid(row = seq_len(nrow(x)),
                     col = seq_len(ncol(x)),
                     character = as.character(NA),
                     double = as.double(NA),
                     integer = as.integer(NA),
                     logical = as.logical(NA),
                     stringsAsFactors = FALSE)
  out$list <- vector("list", nrow(out))
  x.row.names <- row.names(x)
  x.col.names <- colnames(x)
  if (!is.null(x.row.names)) {
    out$col <- out$col + 1L
    out <- rbind(out,
                 blank_data_frame(row = seq_along(x.row.names),
                                  names = x.row.names))
  }
  if (!is.null(x.col.names)) {
    out$row <- out$row + 1L
    out <- rbind(out,
                 blank_data_frame(col = seq_along(x.col.names) + !is.null(x.row.names),
                                  names = x.col.names))
  }
  dim(x) <- NULL # efficiently flatten to a vector
  is_data <- out$row != !is.null(x.col.names) & out$col != !is.null(x.row.names)
  suppressWarnings(out[[typeof(x)]][is_data] <- x)
  tibble::as_tibble(out)
}

#' @export
tidytable.data.frame <- function(x, rownames = TRUE, colnames = TRUE) {
  tidytable.matrix(as.matrix(x), rownames, colnames)
}
