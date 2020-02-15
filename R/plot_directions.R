
#' Plots tidyxl data frame in a grid layout
#' @description
#' This function plots the excel sheet, highlighting the relationship between headers and table values.
#' @param sheet tidyxl data frame
#' @param text text variable to be represented in values plot.
#' @param interactive TRUE produces an interactive plotly visualisation.
#' @export
#' @examples print("todo")

plot_cells <- function(sheet, text = values, interactive = FALSE) {

   if(!is.null(attr(sheet,"data_cells"))){
    
    data_cells <-
      sheet %>%
      attr("data_cells") %>%
      dplyr::mutate(.direction = "\U2610", .header_label = "data")
    
    sheet <- dplyr::bind_rows(sheet, data_cells)
    
   }else{
     
     # Add annotation variables if missing  
     added_var_list <- list(sheet,".header_label",".direction", ".value")
     sheet <-  added_var_list %>% purrr::reduce(add_variable_if_missing)
     
     sheet <- 
       sheet %>% 
       dplyr::mutate(.header_label = "None")
     
   }
  
  if (interactive == FALSE) {
    sheet_01 <-
      sheet %>% dplyr::left_join(unpivotr::direction_plot_noninteractive, by = ".direction") %>% 
      dplyr::mutate(values = dplyr::coalesce(as.character(numeric),as.character(character),
                                             as.character(logical),as.character(date)))
    dplyr::bind_rows(
      dplyr::mutate(sheet_01, .arrow = NA, set = "Cell values"),
      dplyr::mutate(sheet_01, {{text}} := NA, set = "Directions")) %>%
      ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = .header_label)) + ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = stringr::str_sub({{text}}, 1, 3))) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.rotate ==0 ,.arrow, NA))) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.rotate %in% 90,  .arrow, NA)), angle = 90) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.rotate %in% -90, .arrow, NA)), angle = -90) +
      ggplot2::facet_wrap(~set, scales = "free") + 
      ggplot2::labs(y = "Row", x = "Column") +
      ggplot2::scale_y_reverse()
    
  } else {
    
    sheet_01 <-
      sheet %>% dplyr::left_join(unpivotr::direction_plot_interactive, by = ".direction") %>% 
      dplyr::mutate(values = dplyr::coalesce(as.character(numeric),as.character(character),
                                             as.character(logical),as.character(date)))
    plot_object <-
      dplyr::bind_rows(
        dplyr::mutate(sheet_01, .arrow = NA, set = "Cell values"),
        dplyr::mutate(sheet_01, {{text}} := NA, set = "Directions")
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = .header_label)) + ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = stringr::str_sub({{text}}, 1, 5))) +
      ggplot2::geom_text(ggplot2::aes(label = .arrow)) +
      ggplot2::facet_wrap(~set, scales = "free") +
      ggplot2::labs(y = "Row", x = "Column") +
      ggplot2::scale_y_reverse()
    
    plotly::ggplotly(plot_object)
  }
}
