#' Display cells as though in a spreadsheet
#'
#' @description Takes the 'melted' output of `as_cells()` or
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
#' row/col 1(A) 2(B)
#'       1 "a"  "b"
#'       2 "c"  "d"
#' ```
#'
#' The letters in the column names are for comparing this view with a
#' spreadsheet application.
#'
#' @param cells Data frame or tbl, the cells to be displayed.
#' @param values Optional. The column of `cells` to use as the values of each
#' cell.  Given as a bare variable name.  If omitted (the default), the `types`
#' argument will be used instead.
#' @param types The column of `cells` that names, for each cell, which column to
#' use for the value of the cell.  E.g.  a cell with a character value will have
#' `"character"` in this column.
#' @param formatters A named list of functions to format cell values for
#' display, named according to the column that the cell value is in.
#'
#' @export
#' @examples
#' x <- data.frame(name = c("Matilda", "Nicholas"),
#'                 score = c(14L, 10L),
#'                 stringsAsFactors = FALSE)
#'
#' # This is the original form of the table, which is easy to read.
#' x
#'
#' # This is the 'tidy' arrangement that is difficult for humans to read (but
#' # easy for computers)
#' y <- as_cells(x, col_names = TRUE)
#' y
#'
#' # rectify() projects the cells as a spreadsheet again, for humans to read.
#' rectify(y)
#'
#' # You can choose to use a particular column of the data
#' rectify(y, values = chr)
#' rectify(y, values = int)
#'
#' # You can also show which row or which column each cell came from, which
#' # helps with understanding what this function does.
#' rectify(y, values = row)
#' rectify(y, values = col)
#'
#' # Empty rows and columns up to the first occupied cell are dropped, but the
#' # row and column names reflect the original row and column numbers.
#' y$row <- y$row + 5
#' y$col <- y$col + 5
#' rectify(y)
#'
#' # Supply named functions to format cell values for display.
#' rectify(y, formatters = list(chr = toupper, int = ~ . * 10))
rectify <- function(cells, values = NULL, types = data_type,
                    formatters = list()) {
  UseMethod("rectify")
}

#' @export
rectify.data.frame <- function(cells, values = NULL, types = data_type,
                               formatters = list()) {
  check_distinct(cells)
  if (nrow(cells) == 0L) return(tibble::tibble())
  values <- rlang::enexpr(values)
  types <- rlang::ensym(types)
  cells <- pad(cells)
  if (is.null(values)) {
    out <-
      cells %>%
      pack() %>%
      dplyr::select(row, col, value) %>%
      unpack() %>%
      spatter(col, types = data_type, formatters = formatters)
  } else {
    cells <- dplyr::select(cells, row, col, !!values)
    if (rlang::expr_text(values) == "row") {
      cells$.row <- cells$row
      values <- rlang::sym(".row")
    }
    out <- spatter(cells, col, values = !!values, types = !!types)
  }
  # Amend the headers
  minrow <- min(cells$row)
  mincol <- min(cells$col)
  nrows <- max(cells$row) - minrow + 1L
  ncols <- max(cells$col) - mincol + 1L
  colnums <- seq_len(ncols) + mincol - 1L
  colnums <- paste0(colnums, "(", cellranger::num_to_letter(colnums), ")")
  rownums <- seq_len(nrows) + minrow - 1L
  colnames(out) <- c("row/col", colnums)
  out$`row/col` <- rownums
  class(out) <- c("cell_grid", class(out))
  out
}

#' @describeIn rectify S3 method for class `cell_grid`
#' @param x The output of [rectify()]
#' @param display One of `"terminal"` (default), `"browser"`, `"rstudio"`.  To
#' display in the browser you must have the `DT` package installed.
#' @param ... Arguments passed on to [print()]
#' @export
#' @examples
#' #
#' # Print in the browser or in the RStudio viewer pane
#' \dontrun{
#'   z <- rectify(y)
#'   print(z, "browser")
#'   print(z, "rstudio")
#' }
print.cell_grid <- function(x, display = "terminal", ...) { # nocov start
  if (display == "terminal") {
    NextMethod(x)
  } else if (display == "browser") {
    if (!("DT" %in% installed.packages())) {
      "You need to install the 'DT' package to view this in the browser."
    }
    DT::datatable(x,
      extensions = c(
        "FixedHeader",
        "KeyTable",
        "Scroller"
      ),
      options = list(
        paging = FALSE,
        fixedHeader = TRUE,
        keys = TRUE
      ),
      rownames = FALSE
    )
  } else if (display == "rstudio") {
    View(x)
  } else {
    NextMethod(x)
  }
} # nocov end

pad <- function(cells, rows = cells$row, cols = cells$col) {
  padding <- tidyr::crossing(
    row = tidyr::full_seq(c(cells$row, rows), 1L),
    col = tidyr::full_seq(c(cells$col, cols), 1L)
  )
  dplyr::full_join(padding, cells, by = c("row", "col"))
}
