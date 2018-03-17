#' Move sentinel values into a separate column leaving NA behind
#'
#' @export
#' @examples
#' isolate_sentinels(Formaldehyde, 0.3, carb)
#' isolate_sentinels(Formaldehyde, 0.3, carb, flag)
isolate_sentinels <- function(.data, sentinels, col, into = "sentinel") {
  col <- rlang::ensym(col)
  into <- rlang::ensym(into)
  if(typeof(dplyr::pull(.data, !! col)) != typeof(sentinels)) {
    stop("The vector `sentinels` must be the same type (e.g. character, numeric) as `col`")
  }
  dplyr::mutate(.data,
         !! into := dplyr::if_else(!! col %in% sentinels,
                            !! col,
                            na_of_type(!! col)),
         !! col := dplyr::if_else(is.na(!! into), !! col, na_of_type(!! col)))
}
