

#' get_tabledata
#'
#' Extracts the numeric data from the table.
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#'


get_tabledata <- function(sheet, value_ref) {
  sheet %>%
    dplyr::filter(
      !is_blank | !is.na(comment),
      row <= value_ref$max_row,
      row >= value_ref$min_row,
      col <= value_ref$max_col,
      col >= value_ref$min_col
    ) %>%
    dplyr::mutate(value = dplyr::coalesce(as.character(numeric), as.character(character), as.character(logical), as.character(date))) %>%
    dplyr::select(row, col, value, comment)
}
