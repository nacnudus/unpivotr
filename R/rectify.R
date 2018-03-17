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
#' @param col Optional. The column of `cells` to use as the values of each
#' cell.  Given as a bare variable name.  If omitted (the default), the `types`
#' argument will be used instead.
#' @param types The column of `cells` that names, for each cell, which column to
#' use for the value of the cell.  E.g.  a cell with a character value will have
#' `"character"` in this column.  If this argument is given, then `...` will be
#' ignored.
#'
#' @export
#' @examples
#' BOD
#' x <- tidy_table(BOD, TRUE, TRUE)
#' x
#' rectify(x)
#'
#' # You can choose to use a particular column of the data
#' rectify(x, chr)
#' rectify(x, dbl)
#'
#' # You can also show which row or which column each cell came from, which
#' # helps with understanding what this function does.
#' rectify(x, row)
#' rectify(x, col)
#'
#' # Empty rows and columns up to the first occupied cell are dropped, but the
#' # row and column names reflect the original row and column numbers.
#' x$row <- x$row + 5
#' x$col <- x$col + 5
#' rectify(x)
rectify <- function(cells, col, types = data_type) {
  col <- rlang::ensym(col)
  if(rlang::is_missing(col)) {
    types <- rlang::ensym(types)
    unique_types <- unique(dplyr::pull(cells, !! types))
    cells <- dplyr::select(cells,
                           row,
                           col,
                           !! types,
                           !!! rlang::syms(unique_types))
  } else {
    colname <- rlang::expr_text(col)
    types <- rlang::sym("data_type")
    cells <-
      cells %>%
      dplyr::select(row, col, !! col) %>%
      dplyr::mutate(row, col, value = !! col) %>%
      dplyr::mutate(!! types := "value")
  }
  cells <- pack(cells, !! types)
  cells
  cells <- dplyr::mutate(cells, value = purrr::map_chr(value, format_list))
  # Only use used rows/cols
  minrow <- min(cells$row)
  mincol <- min(cells$col)
  nrows <- max(cells$row) - minrow + 1L
  ncols <- max(cells$col) - mincol + 1L
  cells$row = cells$row - minrow + 1L
  cells$col = cells$col - mincol + 1L
  # Create a blank matrix.
  out <- matrix(character(), nrow = nrows, ncol = ncols)
  # Assign to several cells of a matrix cells at once
  out[(cells$col - 1) * nrow(out) + cells$row] <- cells$value
  out
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
#' @param x The output of [rectify()]
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

# Like format() but handles factors wrapped in lists, and leaves NA as-is
format_list <- function(x) {
  if(is.na(x)) return(NA_character_)
  format(x[[1]])
}
