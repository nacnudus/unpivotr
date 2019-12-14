#' paste3
#'
#' Removes NAs from paste. Taken from https://stackoverflow.com/users/1855677/42 stackoverflow answer.
#'
#' @param ... objects to be pasted together.
#' @param sep separating string.
#' @export

paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

#' Produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @param range  a string representing a range in standard excel format. For example, "A4:Z15"
#'
#' @name get_range_df

get_range_df <- function(range){
  
  cell_ref_df <- tibble::as_tibble(cellranger::as.cell_limits(range))
  
  range_df <-
    cell_ref_df[,1:2] %>%
    purrr::set_names(c("min","max")) %>%
    dplyr::mutate(dimension = c("row","col")) %>%
    tidyr::gather(key, value, -dimension) %>%
    tidyr::unite(label, key, dimension, sep = "_") %>%
    tidyr::spread(label, value )
  
  expand.grid(row = c(range_df$min_row[1]:range_df$max_row[1]),
              col = c(range_df$min_col[2]:range_df$max_col[1]))
}

#' Produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range.
#' It handles instances where the range is a union of multiple ranges separate by commas.
#'
#' @param range  a string representing a range in standard excel format. For example, "A4:B15, C11:G22"
#'
#' @name get_range_dfs

get_range_dfs <- function(range){
  
  range %>%
    stringr::str_split(",") %>%
    unlist %>%
    purrr::map(get_range_df) %>%
    dplyr::bind_rows()
}

#' Supply an expression to identify which header groups are hooked.
#'
#' @description
#' Supply an expression to identify which header groups are hooked.
#'
#' @param range  a string representing a range in standard excel format. For example, "A4:B15, C11:G22"
#'
#' @name get_range_dfs

hook <- function(...){
  
  rlang::quos(...)
  
}

#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.

fill_blanks_in_row_headers <- function(header_df, header_fill, formats){
  
  if(header_fill ==  "style"){
    
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = FALSE)
      
      continue <- !identical(sheet_original, header_df)
    }
    
  }
  
  if(header_fill ==  "local_format_id"){
    
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = TRUE)
      
      continue <- !identical(sheet_original, header_df)
    }
    
  }
  
  if(header_fill ==  "borders"){
    
    filled_join <- 
      header_df %>%  
      add_v_border_groups(formats) %>% 
      dplyr::group_by(v_border_group)  %>%  
      dplyr::select(row,col,v_border_group, character) %>% 
      dplyr::mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% stringr::str_remove_all(" _ "),character)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(row,col, character = value) 
    
    header_df <- 
      header_df %>% dplyr::select(-character) %>% dplyr::left_join(filled_join, by = c("row", "col")) %>% 
      dplyr::arrange(row,col) 
    
  
  }
  
  header_df
  
}


#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param df a data frame containing header cells. 
#' @param form_list list of fomulae 
#' @param format the formats associated with the workbook containing the header_df cells.
#' 


fill_blanks_in_headers <- function(header_df, header_fill, formats, direction){
  
  
  if(direction %in% c("N", "S", "up","down")){
    
    header_df <- suppressMessages(fill_blanks_in_col_headers(header_df, header_fill, formats))
    
  }else if(direction %in% c("W", "E", "left","right")){
    
    header_df <- suppressMessages(fill_blanks_in_row_headers(header_df, header_fill, formats))
    
  }else {
    
    stop("Please check the direction you are providing")
    
  }
  
  header_df
  
}















#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param df a data frame containing header cells. 
#' @param form_list list of fomulae 
#' @param format the formats associated with the workbook containing the header_df cells.
#' 

reduce_mutated <- function(df, form_list,format){
  
  current_quosure <-  form_list[[1]]
  var_name_sym <-  rlang::sym(form_list[[2]])
  
  df %>% 
    dplyr::mutate(!!var_name_sym:= !!current_quosure)  
  
}  

#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.
  
  fill_blanks_in_col_headers <- function(header_df,header_fill, formats){
    
    if(header_fill ==  "style"){
      continue <- TRUE
      
      while (continue) {
        sheet_original <- header_df
        header_df <- header_df %>% unmerge_cells(strict_merging = FALSE)
        
        continue <- !identical(sheet_original, header_df)
      }
      
    }
  if(header_fill ==  "local_format_id"){
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = TRUE) 
      
      continue <- !identical(sheet_original, header_df)
    }
    
  }
  
  if(header_fill ==  "borders"){
    
    filled_join <- 
      header_df %>%  
      add_h_border_groups(formats) %>% 
      dplyr::group_by(h_border_group)  %>%  
      dplyr::select(row,col,h_border_group, character) %>% 
      dplyr::mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% 
                              stringr::str_remove_all(" _ "),character)) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(h_border_group, row,col ) %>% 
      dplyr::select(row,col, character = value) 
    
    header_df <- 
      header_df %>% dplyr::select(-character) %>% 
      dplyr::left_join(filled_join, by = c("row", "col")) 
    
  }
  
  header_df  
  
} 

#' Give quasure a name
#'
#' Adds name of a quosure so that use in mutate automatically adds name.
#' @param x quosure
#' @param prefix prefix to be added in name
#'

append_name_to_quosure <- function(x, prefix = "grp_"){
  list(x,
       paste0(prefix,
              x %>% rlang::as_label() %>% make.names() %>% 
                stringr::str_replace_all("\\.+",".") %>% stringr::str_remove_all("(\\.$)|(^X\\.)") %>% 
                stringr::str_replace_all("\\.","_") %>% 
                ifelse(stringr::str_sub(.,start = 1,1) %in% as.character(0:9),paste0("x",.),.  )))
  
}

#' An internal function to add a variable to a tidyxl data frame in that variable is missing 
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param sheet sheet nominated for tidying
#' @param var name of the var to be added.
#'
add_variable_if_missing <- function(sheet, var){
  
  if(!var %in%  names(sheet)){
    var_sym <- rlang::sym(var)
    
    sheet <- sheet %>% dplyr::mutate(!!var_sym := NA_character_)
  }
  
  sheet
}

#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param table_range table_range
#' @param sheet sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'

string_range_to_filter_vars <- function(sheet,table_range){
  
  cell_ref_df <- tibble::as_tibble(cellranger::as.cell_limits(table_range))
  
  table_range_df <-
    cell_ref_df[,1:2] %>%
    purrr::set_names(c("min","max")) %>%
    dplyr::mutate(dimension = c("row","col")) %>%
    tidyr::gather(key, value, -dimension) %>%
    tidyr::unite(label, key, dimension, sep = "_") %>%
    tidyr::spread(label, value )
  
  string_filter_name <- rlang::sym(table_range %>% stringr::str_remove("\\:") %>% paste0("flt_",.))
  
  data_sheet <-
    sheet %>%
    dplyr::mutate(!!string_filter_name := 
             row >= table_range_df$min_row[1] & 
             row <= table_range_df$max_row[1] & 
             col >= table_range_df$min_col[1] &
             col <= table_range_df$max_col[1])
}

#' get_corner_cell_refs
#'
#' Identifies corners of sheet 
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`

get_corner_cell_refs <- function(sheet) {
  
  # Automatic producedure
  ref_df <- 
    sheet %>% 
    dplyr::summarise(
      min_row = min(row), max_row = max(row),
      min_col = min(col), max_col = max(col)
    ) 
  return(ref_df)
  
} 

#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_data a data frame containing header cells. 
#' @param direction direction suplied to enhead. 
#' @param values the data values.

enhead_tabledata <- function(header_data, direction, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction) 
}

#' Get current column number
#'
#' This function gets the maximum col_group number
#'
#' @param labels list of labels. 
#' @param regex_term prefix for col_header. 

get_header_index <- function(labels, regex_term = "^col_header") {
  labels %>% .[stringr::str_detect(.,regex_term)] %>%  stringr::str_remove_all("[a-z]+|[:punct:]+") %>% 
    as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1) 
  
}

#' Order set of excel column indexes.
#'
#' A character vector containing and ordered set of excel column indexes  
#'
#' @format A character vector of length 702
"cols_index"

#' A data frame used for testing. 
#'
#' A dataframe with data_cells added correctly  
#'
#' @format A character vector of length 702
"worked_example_datacells"

#' A data frame used for testin locate_if 
#'
#' A dataframe with location information added correctly using lacate_if  
#'
#' @format A character vector of length 702
"worked_example_locate_if"

#' Matches directions to unicode arrows for an interative chart  
#'
#' A dataframe .direction, .arrow and .rate columns   
#'
#' @format a dataframe
"direction_plot_interactive"

#' Matches directions to unicode arrows for an interative chart  
#'
#' A dataframe .direction, .arrow and .rate columns   
#'
#' @format a dataframe
"direction_plot_noninteractive"
