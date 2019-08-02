
#' plot orientations
#'
#' This function plots the excel sheet, highlighting the relationship between headers and table values.
#' @param orientated_df returned by orientated or orientate_df
#'
#' @examples
#'
#' @export


plot_orientations <- function(orientated_df){
  orientated_df %>% 
    ggplot(aes(
      x = col, y = -row, fill = str_to_title(str_replace_all(.header_group, "_", " ")),
      label = ifelse(.header_group != "data", paste(
        str_extract(.header_group, "[0-9]{1,2}"),
        paste0(ifelse(.header_group != "data", paste0("(", .orientation, ")"), ""))
      ), "")
    )) +
    geom_tile() +
    geom_text(size = 3) +
    theme_minimal() +
    labs(fill = "Cell Type", y = "Row", x = "Column")
}