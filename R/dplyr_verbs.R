#' Add a format variable
#' @description 
#' Add a format variable.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export


append_fmt_single <- function(cells, fmt_function){
  

  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  
    cells %>% 
    dplyr::mutate(!!sym(fmt_function) := rlang::exec(!!sym(fmt_function),
                                                  format_id_vec = cells$local_format_id, 
                                                  sheet_formats = formats)) %>% 
    dplyr::select(!!sym(fmt_function) )
    

}

#' Add multiple format variables
#' @description 
#' Add a format variable.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export


append_fmt <- function(cells, ...){
  
  
  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  argnames <- sys.call()
  
  cells <- sapply(argnames[-c(1,2)], as.character) %>% 
            map(append_fmt_single, cells = cells) %>% 
            bind_cols() %>% 
            bind_cols(cells,.)

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
#' @importMethodsFrom PackageA doWork
#' @export 
setMethod(
  "select", 
  signature = c("tidyxl"), 
  definition =  function(df, ...){
  
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

filter.tidyxl <- function(df, ...){
  
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

mutate.tidyxl <- function(df, ...){
  
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

arrange.tidyxl <- function(df, ...){
  
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

#' Create or transform variables
#' @description 
#' This is mutate from dply 
#' @param ... Name-value pairs of expressions, each with length 1 or the same
#' @export

mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' Select/rename variables by name
#' @description 
#' This is select from dply.
#' @param ... One or more unquoted expressions separated by commas.

#' @export

select <- function(.data, ...) {
  UseMethod("select")
}


#' Return rows with matching conditions
#' @description 
#' Use `filter()` to choose rows/cases where conditions are true. Unlike
#' base subsetting with `[`, rows where the condition evaluates to `NA` are

#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for [tbl_df()], [dtplyr::tbl_dt()] and [dbplyr::tbl_dbi()].
#' @param ... Logical predicates defined in terms of the variables in `.data`.

#' @export

filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}




#' Arrange rows by variables
#' @description 
#' Order tbl rows by an expression involving its variables.
#' @export
#' @param ... Comma separated list of unquoted variable names, or expressions

arrange <- function(.data, ...) {
  UseMethod("arrange")
}

