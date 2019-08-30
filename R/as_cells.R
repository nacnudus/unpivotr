#' Tokenize data frames into a tidy 'melted' structure
#'
#' For certain non-rectangular data formats, it can be useful to parse
#' the data into a melted format where each row represents a single
#' token.
#'
#' @description Data frames represent data in a tabular structure.  [as_cells()]
#' takes the row and column position of each 'cell', and returns that
#' information in a new data frame, alongside the content and type of each cell.
#'
#' This makes it easier to deal with complex or non-tabular data (e.g. pivot
#' tables) that have been imported into R as data frames.  Once they have been
#' 'melted' by [as_cells()], you can use functions like [behead()] and
#' [spatter()] to reshape them into conventional, tidy, unpivoted structures.
#'
#' For HTML tables, the content of each cell is returned as a standalone HTML
#' string that can be further parsed with tools such as the rvest package.  This
#' is particularly useful when an HTML cell itself contains an HTML table, or
#' contains both text and a URL.  If the HTML itself is poorly formatted, try
#' passing it through the
#' [htmltidy](https://CRAN.R-project.org/package=htmltidy) package first.
#'
#' This is an S3 generic.
#'
#' @param x A data.frame or an HTML document
#' @param col_names Whether to treat the column names  as cells, Default: FALSE
#' @param row_names Whether to treat the row names as cells, Default: FALSE
#'
#' @return A data.frame with the following columns:
#'
#' * `row` and `col` (integer) giving the original position of the 'cells'
#' * any relevant columns for cell values in their original types: `chr`,
#'   `cplx`, `cplx`, `dbl`, `fct`, `int`, `lgl`, `list`, and `ord`
#' * `data_type` to specify for each cell which of the above columns (`chr`
#'   etc.) the value is in.
#'
#' The columns `fct` and `ord` are, like `list`, list-columns (each element is
#' independent) to avoid factor levels clashing.  For HTML tables, the column
#' `html` gives the HTML string of the original cell.
#'
#' Row and column names, when present and required by `row_names = TRUE` or
#' `col_names = TRUE`, are treated as though they were cells in the table, and
#' they appear in the `chr` column.
#'
#' @examples
#' x <- data.frame(a = c(10, 20),
#'                 b = c("foo", "bar"),
#'                 stringsAsFactors = FALSE)
#' x
#' as_cells(x)
#' as_cells(x, row_names = TRUE)
#' as_cells(x, col_names = TRUE)
#'
#' # 'list' columns are undisturbed
#' y <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE)
#' y$b <- list(1:2, 3:4)
#' y
#' as_cells(y)
#'
#' # Factors are preserved by being wrapped in lists so that their levels don't
#' # conflict.  Blanks are NULLs.
#' z <- data.frame(x = factor(c("a", "b")),
#'                 y = factor(c("c", "d"), ordered = TRUE))
#' as_cells(z)
#' as_cells(z)$fct
#' as_cells(z)$ord
#'
#' # HTML tables can be extracted from the output of xml2::read_html().  These
#' # are returned as a list of tables, similar to rvest::html_table().  The
#' # value of each cell is its standalone HTML string, which can contain
#' # anything -- even another table.
#'
#' colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
#' rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
#' nested <- system.file("extdata", "nested.html", package = "unpivotr")
#'
#' \dontrun{
#' browseURL(colspan)
#' browseURL(rowspan)
#' browseURL(nestedspan)
#' }
#'
#' as_cells(xml2::read_html(colspan))
#' as_cells(xml2::read_html(rowspan))
#' as_cells(xml2::read_html(nested))
#' @export
as_cells <- function(x, row_names = FALSE, col_names = FALSE) {
  UseMethod("as_cells")
}

#' @export
as_cells.data.frame <- function(x, row_names = FALSE, col_names = FALSE) {
  values <- do.call(c, purrr::map(x, as.list))
  nrows <- nrow(x)
  ncols <- ncol(x)
  types <- purrr::map_chr(x, pillar::type_sum)
  # Spread cells into different columns by data type
  out <- tibble::tibble(
    row = rep.int(seq_len(nrow(x)), ncols),
    col = rep(seq_len(ncol(x)), each = nrows),
    value = values,
    type = rep(types, each = nrows),
    data_type = type
  )
  out <- tidyr::spread(out, type, value)
  if (row_names) {
    rnames <- row.names(x)
    out$col <- out$col + 1L
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        col = 1L,
        row = seq_along(rnames),
        chr = vctrs::vec_cast(rnames, list()),
        data_type = "chr"
      )
    )
  }
  if (col_names) {
    cnames <- colnames(x)
    out$row <- out$row + 1L
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        row = 1L,
        col = seq_along(cnames) + row_names,
        chr = vctrs::vec_cast(cnames, list()),
        data_type = "chr"
      )
    )
  }
  # Convert non-list-columns to vectors
  out <- dplyr::mutate_at(out,
    tidyselect::vars_select(
      names(out),
      dplyr::everything(),
      .exclude = c("row", "col", "data_type")
    ),
    concatenate,
    combine_factors = FALSE,
    fill_factor_na = FALSE
  )
  # Append row and column names
  out <- dplyr::select(out, row, col, data_type, sort(colnames(out)))
  dplyr::arrange(out, col, row)
}

grow_matrix <- function(x, i, j, value) {
  dim_x <- dim(x)
  grow <- c(i, j) > dim_x
  if (any(grow)) {
    new_i <- if (grow[1]) dim_x[1] * 2 else dim_x[1]
    new_j <- if (grow[2]) dim_x[2] * 2 else dim_x[2]
    x <- do.call(
      what = `[<-`,
      args = list(matrix(value, new_i, new_j),
        i = seq_len(dim_x[1]),
        j = seq_len(dim_x[2]),
        value = x
      )
    )
    return(grow_matrix(x, i, j, value))
  }
  x
}

#' @export
as_cells.xml_node <- function(x, row_names = FALSE, col_names = FALSE) {
  # Remove ancestors
  x <-
    x %>%
    as.character() %>%
    xml2::read_html() %>%
    xml2::xml_find_all(xpath = "//table[not(ancestor::table)]") %>%
    .[[1]]
  rows <- xml2::xml_find_all(x, xpath = "//tr[not(ancestor::tr)]")
  scratch <- matrix(FALSE, nrow = length(rows)) # For marking used cells
  out <- vector(mode = "list")
  i <- 0
  for (row_i in rows) {
    i <- i + 1
    if (is.null(out[i][[1]])) out[i][[1]] <- vector(mode = "character") # Create a row
    j <- 0
    cells <- xml2::xml_find_all(row_i,
      xpath = ".//*[(name()='th' or name()='td') and not(ancestor::th|ancestor::td)]"
    )
    for (cell in cells) {
      j <- j + 1
      rowspan <- xml2::xml_attr(cell, "rowspan")
      colspan <- xml2::xml_attr(cell, "colspan")
      rowspan <- ifelse(is.na(rowspan), 1, as.integer(rowspan))
      colspan <- ifelse(is.na(colspan), 1, as.integer(colspan))
      scratch <- grow_matrix(scratch, i - 1 + rowspan, j - 1 + colspan, FALSE)
      while (scratch[i, j]) {
        # skip cells already occupied by merged cells above, as marked in the
        # scratch matrix
        j <- j + 1
        scratch <- grow_matrix(scratch, i - 1 + rowspan, j - 1 + colspan, FALSE)
      }
      # Assign the html of the cell
      out[[i]][j] <- as.character(cell)
      # Mark all used cells, including all parts of merged cells
      scratch[
        seq(i, length.out = rowspan),
        seq(j, length.out = colspan)
      ] <- TRUE
    }
  }
  # Give all rows the same number of columns
  maxcols <- max(purrr::map_int(out, length))
  out <- purrr::map(
    out,
    function(.x) {
      length(.x) <- maxcols
      .x
    }
  )
  # Convert to a tibble, and then cells
  out <- purrr::transpose(out)
  out <- purrr::map(out, purrr::flatten_chr)
  out <- tibble::set_tidy_names(out, quiet = TRUE)
  out <- tibble::as_tibble(out, .name_repair = "minimal")
  out <- as_cells(out, row_names = FALSE, col_names = FALSE)
  out[, c("double", "integer", "logical")] <- NULL
  colnames(out) <- c("row", "col", "data_type", "html")
  out$data_type <- "html"
  dplyr::arrange(out, col, row)
}

#' @export
as_cells.xml_document <- function(x, row_names = FALSE, col_names = FALSE) {
  tables <- xml2::xml_find_all(x, xpath = "//table[not(ancestor::table)]")
  lapply(tables, as_cells)
}
