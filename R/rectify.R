#' Display cells as though in a spreadsheet
#'
#' @description Takes the 'melted' output of `tidy_table()` or
#' `tidyxl::xlsx_cells()` (each row represents one cell) and projects the cells
#' into their original positions.  By default this prints to the
#' terminal/console, but with `display = "browser"` or `display = "rstudio"` it
#' will be displayed in the browser or the RStudio viewer pane.
#'
#' This is for viewing only; the output is not designed to be used in other
#' functions.
#'
#' Example: The following cells
#'
#' ```
#' row col value
#'   1   1   "a"
#'   1   2   "b"
#'   2   1   "c"
#'   2   2   "d"
#' ```
#'
#' Would be presented as
#'
#' ```
#'   1(A) 2(B)
#' 1 "a"  "b"
#' 2 "c"  "d"
#' ```
#'
#' The letters in the column names are for comparing this view with a
#' spreadsheet application.
#'
#' @param cells Data frame or tbl, the cells to be displayed.
#' @param ... The columns of `cells` to use as the values of each cell.  Given
#' as bare variable names.  If you list more than one column, then the value
#' that is chosen for each cell will be the column that isn't `NA`.  You should
#' ensure that only one column isn't `NA` for each cell, otherwise the result
#' will be unpredictable.  Alternatively, use the `values` argument to
#' choose a column for each cell.
#' @param values The column of `cells` that names, for each cell, which
#' column to use for the value of the cell.  If this argument is given, then
#' `...` will be ignored.
#'
#' @export
#' @examples
#' x <- data.frame(row = c(1L, 1L, 2L, 2L),
#'                 col = c(1L, 2L, 1L, 2L),
#'                 value = letters[1:4])
#' x
#'
#' # Typical usage is to choose a 'value' of the cell to represent in the grid
#' rectify(x, value)
#'
#' # You can also show which row or which column each cell came from, which
#' # helps with understanding what this function does.
#' rectify(x, row)
#' rectify(x, col)
#'
#' # If some cells have a character value, and others contain a numeric
#' # value, you can use both at once.  As long as no cell has a non-NA value in
#' # more than one of the columns you choose to represent in the grid, you'll be
#' # okay.
#' mtcars_tidy <- tidy_table(mtcars, TRUE, TRUE)
#' print(dplyr::arrange(mtcars_tidy, row, col), n = 20)
#' rectify(mtcars_tidy, dbl) # use only numeric values
#' rectify(mtcars_tidy, chr) # use only character values
#' rectify(mtcars_tidy, chr, dbl) # use both numeric and character values
#' rectify(mtcars_tidy, dbl, chr) # same as above
#'
#' # This is what happens if cells have non-NA values in more than one of the
#' # colums you choose to represent in the grid.  Here, the row numbers are
#' # treated as cell values.  Every cell has a row number, which is what you see
#' # in the grid.  But the 'chr' column is also used, so non-NA values in the
#' # 'chr' column override the value from the 'row' column.  Currently the last
#' # column to be specified wins, but that behaviour is not guaranteed for
#' # future releases.
#' rectify(mtcars_tidy, row, chr)
#' rectify(mtcars_tidy, chr, row)
#'
#' # Empty rows and columns up to the first occupied cell are dropped.
#' x$row <- x$row + 5
#' x$col <- x$col + 5
#' rectify(x, value)
rectify <- function(cells, ..., values = NULL) {
  columns <- rlang::ensyms(...)
  # Silently omit columns not in `cells`
  columns <- intersect(columns, rlang::syms(colnames(cells)))
  if(length(columns) == 0) {
    return(matrix(numeric(), 0, 0))
  }
  # Only use used rows/cols, and allow an extra row and col for headers
  minrow <- min(cells$row)
  mincol <- min(cells$col)
  nrows <- max(cells$row) - minrow + 1L
  ncols <- max(cells$col) - mincol + 1L
  cells$row = cells$row - minrow + 1L
  cells$col = cells$col - mincol + 1L
  # Convert factors to character
  cells <- dplyr::mutate_if(cells, is.factor, as.character)
  # Create a blank matrix.  Type is character unless all the columns of `cells`
  # are the same type and are not character.
  types <- purrr::map_chr(dplyr::select(cells, !!!columns), typeof)
  same_types <- length(unique(types)) == 1
  blank <- (if(same_types) rlang::eval_tidy(columns[[1]], cells)[0] else character())
  out <- matrix(blank, nrow = nrows, ncol = ncols)
  # Assign the columns of `cells` to the matrix, converting to character if
  # necessary
  for (column in columns) {
    value_cells <- dplyr::filter(cells, !is.na(!!column))
    values <- rlang::eval_tidy(column, value_cells)
    if(!same_types) {
      values <- format(values, justify = "none")
    }
    # Assign to several cells of a matrix cells at once
    out[(value_cells$col - 1) * nrow(out) + value_cells$row] <- values
  }
  # Put in the headers
  colnums <- seq_len(ncols) + mincol - 1L
  colnums <- paste0(colnums, "(", cellranger::num_to_letter(colnums), ")")
  rownums <- seq_len(nrows) + minrow - 1L
  colnames(out) <- colnums
  rownames(out) <- rownums
  class(out) <- c("cell_grid", class(out))
  out
}

#' @describeIn rectify S3 method for class `cell_grid`
#' @param display One of `"terminal"` (default), `"browser"`, `"rstudio"`.  To
#' display in the browser you must have the `DT` package installed.
#' @export
#' @examples
#' #
#' # Print in the browser or in the RStudio viewer pane
#' \dontrun{
#'   y <- rectify(mtcars_tidy, chr, dbl)
#'   print(y, "browser")
#'   print(y, "rstudio")
#' }
print.cell_grid <- function(x, display = "terminal", ...) { # nocov start
  if (display == "terminal") {
    # Print with maximum terminal width
    width <- options()$width
    options(width = 10000)
    NextMethod(x)
    options(width = width)
  } else if (display == "browser") {
    if (!("DT" %in% installed.packages())) {
      "You need to install the 'DT' package to view this in the browser."
    }
    headers <- c(".", colnames(x)) # empty names disallowed
    DT::datatable(x,
                  extensions = c("FixedHeader",
                                 "KeyTable",
                                 "Scroller"),
                  options = list(paging = FALSE,
                                 fixedHeader = TRUE,
                                 keys = TRUE),
                  colnames = headers)
  } else if (display == "rstudio") {
    View(x)
  } else {
    NextMethod(x)
  }
} # nocov end
