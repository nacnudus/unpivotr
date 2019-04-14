#' Manually add orientation information to a tidyxl data frame.
#'
#' @description
#' This function adds a two columns: .orientation and .header_group.
#' .orientation indicates the directional relationship between the current cell and the data in the middle of the table.
#' .header_group represents the name of the header group.
#'
#' @param tidyxl_df  a tidyxl data frame
#' @param orientations  a vector of directions/data indicators with corresponding ranges as names.
#' For example:  c("C1:D1" = "N",  "C2:D2" = "N", "B3:B4" = "W" ,  "A3:A4" = "W",  "c3:d4" = "data")
#'
#' @name orientate
#' @export
#' @examples

orientate <- function(tidyxl_df, orientations){
  orientations_df <-
    orientations %>%
    get_orientations_df()

  tidyxl_df %>%
    left_join(orientations_df, by = c("row", "col"))

}

