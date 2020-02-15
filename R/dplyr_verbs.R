#' Add a format variable
#' @description 
#' Add a format variable.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export
#' @examples print("todo")


append_fmt_single <- function(cells, fmt_function){
  

  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  
    cells %>% 
    dplyr::mutate(!!rlang::sym(fmt_function) := rlang::exec(!!rlang::sym(fmt_function),
                                                  format_id_vec = cells$local_format_id, 
                                                  sheet_formats = formats)) %>% 
    dplyr::select(!!rlang::sym(fmt_function) )
    

}

#' Add multiple format variables
#' @description 
#' Add a format variable.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param ... fmt_* function. 
#' @export
#' @examples print("todo")


append_fmt <- function(cells, ...){
  
  
  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  argnames <- sys.call()
  
  cells <- sapply(argnames[-c(1,2)], as.character) %>% 
    purrr::map(append_fmt_single, cells = cells) %>% 
    dplyr::bind_cols() %>% 
    dplyr::bind_cols(cells,.)
  
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
#' @examples print("todo")

select_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  class_orginal <-class(df)
  class(df) <- class_orginal[-1]
  
  select_quos <-   rlang::quos(...) 
  
  df <- dplyr::select(df,!!!select_quos)
  
  class(df) <- class_orginal
  
  
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
#' @examples print("todo")

filter_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  class_orginal <-class(df)
  class(df) <- class_orginal[-1]
  
  
  filter_quos <-   rlang::quos(...) 
  
  df <- dplyr::filter(df,!!!filter_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  class(df) <- class_orginal
  
  
  df  
}


#' Select keeping formats 
#' @description 
#' A wrapper for dplyr::mutate that retains formatting information 
#' @param df a data frame created by tidyxl::xlsx_cells 
#' @param ... select input. 
#' @export 
#' @examples print("todo")

mutate_fmt <-  function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  class_orginal <-class(df)
  class(df) <- class_orginal[-1]
  
  select_quos <-   rlang::quos(...) 
  
  df <- dplyr::mutate(df,!!!select_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  class(df) <- class_orginal
  
  df  
}


#' Select keeping formats 
#' @description 
#' A wrapper for dplyr::arrange that retains formatting information 
#' @param df a data frame created by tidyxl::xlsx_cells 
#' @param ... select input. 
#' @export 
#' @examples print("todo")

arrange_fmt <-  function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  class_orginal <-class(df)
  class(df) <- class_orginal[-1]
  
  select_quos <-   rlang::quos(...) 
  
  
  df <- dplyr::arrange(df,!!!select_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  class(df) <- class_orginal
  
  df  
}


