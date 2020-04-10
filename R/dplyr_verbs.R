#' Add a format variable
#' @description 
#' Add a variable indicating formatting of cells.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export
#' @examples 
#' 
#'  
#' library(tidyverse)
#'
#' # Read in a formatted tidyxl data frame. 
#' 
#' xl_df <- 
#'  unpivotr_example("worked-examples.xlsx") %>% 
#'  xlsx_cells_fmt(sheets = "pivot-hierarchy")
#'  
#' # Add a column indicate the leveling of indenting for each cell and locate data cell.
#' xl_df <- 
#'  xl_df %>% 
#'   append_fmt(fmt_alignment_indent) %>%
#'   locate_data(data_type == "numeric") 
#' # Add annotations for header cells. First for header cells to the left of the table with no indenting, and then for cells for one level of indenting.
#' xl_df <- 
#'  xl_df %>% 
#'   locate_if(fmt_alignment_indent == 0, direction = "WNW", name = subject_type) %>% 
#'   locate_if(fmt_alignment_indent == 1, direction = "W", name = subject) %>% 
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


append_fmt_single <- function(cells, fmt_function){
  

  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  
    cells %>% 
    dplyr::mutate(!!rlang::sym(fmt_function) := rlang::exec(!!rlang::sym(fmt_function),
                                                  format_id_vec = cells$local_format_id, 
                                                  sheet_formats = formats)) %>% 
    dplyr::select(!!rlang::sym(fmt_function) )
    

}

#' Add a format variable
#' @description 
#' Add a variable indicating formatting of cells.
#' @param cells a data frame created by tidyxl::xlsx_cells 
#' @param fmt_function fmt_* function. 
#' @export
#' @examples 
#' 
#'  
#' library(tidyverse)
#'
#' # Read in a formatted tidyxl data frame. 
#' 
#' xl_df <- 
#'  unpivotr_example("worked-examples.xlsx") %>% 
#'  xlsx_cells_fmt(sheets = "pivot-hierarchy")
#'  
#' # Add a column indicate the leveling of indenting for each cell and locate data cell.
#' xl_df <- 
#'  xl_df %>% 
#'   append_fmt(fmt_alignment_indent) %>%
#'   locate_data(data_type == "numeric") 
#' # Add annotations for header cells. First for header cells to the left of the table with no indenting, and then for cells for one level of indenting.
#' xl_df <- 
#'  xl_df %>% 
#'   locate_if(fmt_alignment_indent == 0, direction = "WNW", name = subject_type) %>% 
#'   locate_if(fmt_alignment_indent == 1, direction = "W", name = subject) %>% 
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


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
#' @examples 
#'  
#' library(tidyverse)
#'
#' 
#' annotated_df <- 
#' unpivotr_example("newresconst.xlsx") %>% 
#'   xlsx_cells_fmt(sheets = "Table 1 - Permits") %>%
#'   append_fmt(fmt_font_bold) %>% 
#'   filter_fmt(row < min(row[str_detect(character,"RSE")],na.rm = TRUE)) %>% 
#'   select_fmt(-height) %>% 
#'   locate_data(data_type == "numeric" & col > 1) %>%
#'   locate_groups(direction = "W", 
#'                 .groupings = groupings(is.na(numeric)), 
#'                 .hook_if = hook(any(data_type == "numeric"))) %>% 
#'   locate_groups(direction = "N", header_fill = "style")  
#' 
#'  annotated_df %>% plot_cells()

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
#' @examples 
#'  
#' library(tidyverse)
#'
#' annotated_df <- 
#'  unpivotr_example("newresconst.xlsx") %>% 
#'   xlsx_cells_fmt(sheets = "Table 1 - Permits") %>%
#'   append_fmt(fmt_font_bold) %>% 
#'   filter_fmt(row < min(row[str_detect(character,"RSE")],na.rm = TRUE)) %>% 
#'   select_fmt(-height) %>% 
#'   locate_data(data_type == "numeric" & col > 1) %>%
#'   locate_groups(direction = "W", 
#'                 .groupings = groupings(is.na(numeric)), 
#'                 .hook_if = hook(any(data_type == "numeric"))) %>% 
#'   locate_groups(direction = "N", header_fill = "style")  
#' 
#'  annotated_df %>% plot_cells()


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
#' @examples 
#'  
#' library(tidyverse)
#'
#' annotated_df <- 
#' unpivotr_example("newresconst.xlsx") %>% 
#'   xlsx_cells_fmt(sheets = "Table 1 - Permits") %>%
#'   append_fmt(fmt_font_bold) %>% 
#'   filter_fmt(row < min(row[str_detect(character,"RSE")],na.rm = TRUE)) %>% 
#'   mutate_fmt(double_height = height *2) %>% 
#'   locate_data(data_type == "numeric" & col > 1) %>%
#'   locate_groups(direction = "W", 
#'                 .groupings = groupings(is.na(numeric)), 
#'                 .hook_if = hook(any(data_type == "numeric"))) %>% 
#'   locate_groups(direction = "N", header_fill = "style")  
#' 
#'  annotated_df %>% plot_cells()


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
#' @examples
#'   
#' library(tidyverse)
#'
#' annotated_df <- 
#' unpivotr_example("newresconst.xlsx") %>% 
#'   xlsx_cells_fmt(sheets = "Table 1 - Permits") %>%
#'   append_fmt(fmt_font_bold) %>% 
#'   filter_fmt(row < min(row[stringr::str_detect(character,"RSE")],na.rm = TRUE)) %>% 
#'   arrange_fmt(height) %>% 
#'   locate_data(data_type == "numeric" & col > 1) %>%
#'   locate_groups(direction = "W", 
#'                 .groupings = groupings(is.na(numeric)), 
#'                 .hook_if = hook(any(data_type == "numeric"))) %>% 
#'   locate_groups(direction = "N", header_fill = "style")  
#' 
#'  annotated_df %>% plot_cells()

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


