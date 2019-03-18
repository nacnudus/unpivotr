#' Move sentinel values into a separate column leaving NA behind
#'
#' @description A sentinel value, takes the place of a value that isn't
#' available for some reason.  [isolate_sentinels()] removes these values from a
#' column of data into a separate column, and optionally converts the data left
#' behind into an appropriate data type.
#'
#' @param .data A data frame.
#' @param col The name of the column of data containing sentinel values.
#' @param sentinels A vector of sentinel values to be removed.
#' @param into A name to give the new column of sentinel values.
#'
#' @export
#' @examples
#' x <- data.frame(name = c("Matilda", "Nicholas", "Olivia", "Paul"),
#'                 score = c(10, "confidential", "N/A", 12),
#'                 stringsAsFactors = FALSE)
#' x
#' isolate_sentinels(x, score, c("confidential", "N/A"))
#' isolate_sentinels(x, score, c("confidential", "N/A"), "flag")
isolate_sentinels <- function(.data, col, sentinels, into = "sentinel") {
  col <- rlang::ensym(col)
  into <- rlang::ensym(into)
  col_class <- class(dplyr::pull(.data, !!col))[1]
  if (col_class %in% c("factor", "ordered", "list")) {
    stop(
      "Sentinels can't be isolated from a factors or lists (column `",
      rlang::expr_text(col),
      "`). Convert factors to character first, and choose elements of lists to turn into a vector."
    )
  }
  if (col_class != class(sentinels)[1]) {
    stop("The vector `sentinels` must be the same type (e.g. character, numeric) as `col`")
  }
  dplyr::mutate(
    .data,
    !!into := dplyr::if_else(
      !!col %in% sentinels,
      !!col,
      na_of_type(!!col)
    ),
    !!col := dplyr::if_else(is.na(!!into), !!col, na_of_type(!!col))
  )
}
