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
#' For HTML tables, the content of each cell is returned as standalone HTML that
#' can be further parsed with tools such as the rvest package.  This is
#' particularly useful when an HTML cell itself contains an HTML table, or
#' contains both text and a URL, which must be extracted separately.  If the
#' HTML itself is poorly formatted, try passing it through the htmltidy package
#' first.
#'
#' This is an S3 generic.
#'
#' @param x A data.frame or an HTML document
#' @param colnames Whether to include the row names in the output, Default:
#' FALSE
#' @param rownames Whether to include the column names in the output, Default:
#' FALSE
#'
#' @return A data.frame with columns 'row' and 'col' (integer) giving the
#' original position of the 'cells', and any relevant columns for cell values in
#' their original types: 'chr', 'cplx', 'cplx', 'dbl', 'fctr', 'int', 'lgl', and
#' 'list'.  The columns 'fctr' is, like 'list', a list-column (each element is
#' itself a list) to avoid factor levels clashing.  For HTML tables, the column
#' 'html' gives the HTML of the original cell.
#'
#' Row and column names, when present and required, are treated as though they
#' were cells in the table, and they appear in the 'chr' column.
#'
#' @examples
#' tidy_table(Formaldehyde)
#' tidy_table(Formaldehyde, colnames = TRUE)
#' tidy_table(Formaldehyde, rownames = TRUE)
#'
#' # 'list' columns are undisturbed
#' x <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE)
#' x$b <- list(1:2, 3:4)
#' x
#' unpivotr::tidy_table(x)
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
#' tidy_table(xml2::read_html(colspan))
#' tidy_table(xml2::read_html(rowspan))
#' tidy_table(xml2::read_html(nested))
#' @export
tidy_table <- function(x, rownames = FALSE, colnames = FALSE) {
  UseMethod("tidy_table")
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
                       ~ unlist(purrr::map(.x, ~ ifelse(is.null(.x), NA, .x))))
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

grow_matrix <- function(x, i, j, value) {
  dim_x <- dim(x)
  grow <- c(i, j) > dim_x
  if (any(grow)) {
    new_i <- if (grow[1]) dim_x[1] * 2 else dim_x[1]
    new_j <- if (grow[2]) dim_x[2] * 2 else dim_x[2]
    x <- do.call(what = `[<-`,
                 args = list(matrix(value, new_i, new_j),
                             i = seq_len(dim_x[1]),
                             j = seq_len(dim_x[2]),
                             value = x))
    return(grow_matrix(x, i, j, value))
  }
  x
}

#' @export
tidy_table.xml_node <- function(x, rownames = FALSE, colnames = FALSE) {
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
                               xpath = ".//*[(name()='th' or name()='td') and not(ancestor::th|ancestor::td)]")
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
        scratch[seq(i, length.out = rowspan),
                seq(j, length.out = colspan)] <- TRUE
    }
  }
  # Give all rows the same number of columns
  maxcols <- max(purrr::map_int(out, length))
  out <- purrr::map(out,
                  function(.x) {
                    length(.x) <- maxcols
                    .x})
  # Convert to a tibble, and then a tidy_table
  out <- purrr::transpose(out)
  out <- purrr::map(out, purrr::flatten_chr)
  out <- tibble::set_tidy_names(out, quiet = TRUE)
  out <- tibble::as_data_frame(out)
  out <- tidy_table(out, rownames = FALSE, colnames = FALSE)
  out[, c("double", "integer", "logical")] <- NULL
  colnames(out) <- c("row", "col", "html")
  dplyr::arrange(out, col, row)
}

#' @export
tidy_table.xml_document <- function (x, rownames = FALSE, colnames = FALSE) {
  tables <- xml2::xml_find_all(x, xpath = "//table[not(ancestor::table)]")
  lapply(tables, tidy_table)
}

