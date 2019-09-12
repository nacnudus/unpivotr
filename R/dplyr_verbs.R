#' Add a format variable
#' @description 
#' Add a format variable.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export

append_fmt <- function(cells, fmt_function){
  
  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  cells <- 
    cells %>% 
    dplyr::mutate({{fmt_function}} := rlang::exec({{fmt_function}},
                                    format_id_vec = cells$local_format_id, 
                                    sheet_formats = formats))
  attr(cells,"formats") <- formats
  attr(cells,"data_cells") <- data_cells
  
  cells
  
}

#' Select keeping formats 
#' @description 
#' A wrapper for dplyr::select that retains formatting information 
#' @param df a data frame created by tidyxl::xlsx_cells 
#' @param ... select input. 
#' @export

select_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  select_quos <-   rlang::quos(...) 
  
  df <- dplyr::select(df,!!!select_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  df  
}

#' Filter keeping formats 
#' @description 
#' A wrapper for dplyr::filter that retains formatting information 
#' @param df a data frame created by tidyxl::xlsx_cells 
#' @param ... filter expression. 
#' @export

filter_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  filter_quos <-   rlang::quos(...) 
  
  df <- dplyr::filter(df,!!!filter_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  df  
}
