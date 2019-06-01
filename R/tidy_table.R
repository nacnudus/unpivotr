#' Tokenize data frames into a tidy 'melted' structure
#'
#' @description
#'
#' [tidy_table()] will be deprecated.  Use [as_cells()] instead.
#'
#' For certain non-rectangular data formats, it can be useful to parse
#' the data into a melted format where each row represents a single
#' token.
#'
#' Data frames represent data in a tabular structure.  `tidy_table` takes the
#' row and column position of each 'cell', and returns that information in a new
#' data frame, alongside the content and type of each cell.
#'
#' This makes it easier to deal with complex or non-tabular data (e.g. pivot
#' tables) that have been imported into R as data frames.  Once they have been
#' 'melted' by [tidy_table()], you can use functions like [behead()] and
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
#'   `cpl`, `dbl`, `fct`, `int`, `lgl`, `list`, and `ord`
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
#' tidy_table(x)
#' tidy_table(x, row_names = TRUE)
#' tidy_table(x, col_names = TRUE)
#'
#' # 'list' columns are undisturbed
#' y <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE)
#' y$b <- list(1:2, 3:4)
#' y
#' tidy_table(y)
#'
#' # Factors are preserved by being wrapped in lists so that their levels don't
#' # conflict.  Blanks are NULLs.
#' z <- data.frame(x = factor(c("a", "b")),
#'                 y = factor(c("c", "d"), ordered = TRUE))
#' tidy_table(z)
#' tidy_table(z)$fct
#' tidy_table(z)$ord
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
#' tidy_table(xml2::read_html(colspan))
#' tidy_table(xml2::read_html(rowspan))
#' tidy_table(xml2::read_html(nested))
#' @export
tidy_table <- function(x, row_names = FALSE, col_names = FALSE) {
  message("tidy_table() will be deprecated.  Use as_cells() instead.")
  UseMethod("as_cells")
}
