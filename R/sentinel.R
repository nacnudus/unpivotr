#' Move sentinel values into a separate column leaving NA behind
#'
#' @export
#' @examples
#' isolate_sentinels(Formaldehyde, 0.3, carb)
#' isolate_sentinels(Formaldehyde, 0.3, carb, flag)
isolate_sentinels <- function(.data, col, sentinels, into = "sentinel") {
  col <- rlang::ensym(col)
  into <- rlang::ensym(into)
  col_class <- class(dplyr::pull(.data, !! col))[1]
  if(col_class %in% c("factor", "ordered", "list")) {
    stop("Sentinels can't be isolated from a factors or lists (column `",
         rlang::expr_text(col),
         "`). Convert factors to character first, and choose elements of lists to turn into a vector.")
  }
  if(col_class != class(sentinels)[1]) {
    stop("The vector `sentinels` must be the same type (e.g. character, numeric) as `col`")
  }
  dplyr::mutate(.data,
         !! into := dplyr::if_else(!! col %in% sentinels,
                            !! col,
                            na_of_type(!! col)),
         !! col := dplyr::if_else(is.na(!! into), !! col, na_of_type(!! col)))
}
