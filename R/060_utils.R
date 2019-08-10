#' paste3
#'
#' Removes NAs from paste. Taken from https://stackoverflow.com/users/1855677/42 stackoverflow answer.
#'
#' @param table_components object returned by `process_ABS_sheet`
#'
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
  
  cell_ref_df <- as_tibble(cellranger::as.cell_limits(range))
  
  range_df <-
    cell_ref_df[,1:2] %>%
    set_names(c("min","max")) %>%
    mutate(dimension = c("row","col")) %>%
    gather(key, value, -dimension) %>%
    unite(label, key, dimension, sep = "_") %>%
    spread(label, value )
  
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
    str_split(",") %>%
    unlist %>%
    map(get_range_df) %>%
    bind_rows()
}



#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
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
      group_by(h_border_group)  %>%  
      select(row,col,h_border_group, character) %>% 
      mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% 
                              str_remove_all(" _ "),character)) %>% 
      ungroup() %>% 
      arrange(h_border_group, row,col ) %>% 
      select(row,col, character = value) 
    
    header_df <- 
      header_df %>% select(-character) %>% 
      left_join(filled_join, by = c("row", "col")) 
    
  }
  
  header_df  
  
} 


#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
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
      group_by(v_border_group)  %>%  
      select(row,col,v_border_group, character) %>% 
      mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% str_remove_all(" _ "),character)) %>% 
      ungroup() %>% 
      select(row,col, character = value) 
    
    header_df <- 
      header_df %>% select(-character) %>% left_join(filled_join, by = c("row", "col")) %>% 
      arrange(row,col) 
    
    
    
  }
  
  header_df
  
}


#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.
#' 


reduce_mutated <- function(df, form_list,format){
  
  current_quosure <-  form_list[[1]]
  var_name_sym <-  sym(form_list[[2]])
  
  df %>% 
    mutate(!!var_name_sym:= !!current_quosure)  
  
}  




#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
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
      group_by(h_border_group)  %>%  
      select(row,col,h_border_group, character) %>% 
      mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% 
                              str_remove_all(" _ "),character)) %>% 
      ungroup() %>% 
      arrange(h_border_group, row,col ) %>% 
      select(row,col, character = value) 
    
    header_df <- 
      header_df %>% select(-character) %>% 
      left_join(filled_join, by = c("row", "col")) 
    
  }
  
  header_df  
  
} 








#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
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
      group_by(v_border_group)  %>%  
      select(row,col,v_border_group, character) %>% 
      mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% str_remove_all(" _ "),character)) %>% 
      ungroup() %>% 
      select(row,col, character = value) 
    
    header_df <- 
      header_df %>% select(-character) %>% left_join(filled_join, by = c("row", "col")) %>% 
      arrange(row,col) 
    
    
    
  }
  
  header_df
  
}



#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'

append_name_to_quosure <- function(x, prefix = "grp_"){
  list(x,
       paste0(prefix,
              x %>% as_label() %>% make.names() %>% 
                str_replace_all("\\.+",".") %>% str_remove_all("(\\.$)|(^X\\.)") %>% 
                str_replace_all("\\.","_") %>% 
                ifelse(str_sub(.,start = 1,1) %in% as.character(0:9),paste0("x",.),.  )))
  
}



#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'



add_variable_if_missing <- function(sheet, var){
  
  if(!var %in%  names(sheet)){
    var_sym <- sym(var)
    
    sheet <- sheet %>% mutate(!!var_sym := NA_character_)
  }
  
  sheet
}



#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
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
  
  cell_ref_df <- as_tibble(cellranger::as.cell_limits(table_range))
  
  table_range_df <-
    cell_ref_df[,1:2] %>%
    set_names(c("min","max")) %>%
    mutate(dimension = c("row","col")) %>%
    gather(key, value, -dimension) %>%
    unite(label, key, dimension, sep = "_") %>%
    spread(label, value )
  
  string_filter_name <- sym(table_range %>% str_remove("\\:") %>% paste0("flt_",.))
  
  data_sheet <-
    sheet %>%
    mutate(!!string_filter_name := 
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
    summarise(
      min_row = min(row), max_row = max(row),
      min_col = min(col), max_col = max(col)
    ) 
  return(ref_df)
  
} 




#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.



enhead_tabledata <- function(header_data, direction, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction) 
}



#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.



get_header_index <- function(labels, regex_term = "^col_header") {
  labels %>% .[str_detect(.,regex_term)] %>%  str_remove_all("[a-z]+|[:punct:]+") %>% 
    as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1) 
  
}


#' Fill in blanks
#' @description 
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.
#' @export

append_fmt <- function(cells, fmt_function){
 
  formats <- attr(cells,"formats")
  data_cells <- attr(cells,"data_cells")
  
  cells <- 
  cells %>% 
    mutate({{fmt_function}} := exec({{fmt_function}},
                                    format_id_vec = cells$local_format_id, 
                                    sheet_formats = formats))
  attr(cells,"formats") <- formats
  attr(cells,"data_cells") <- data_cells
  
  cells
  
}

#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.



select_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  select_quos <-   enquos(...) 
  
  df <- select(df,!!!select_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats

  df  
}
  
#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.




filter_fmt <- function(df, ...){
  
  data_cells <- attr(df,"data_cells")
  formats    <- attr(df,"formats")
  
  filter_quos <-   enquos(...) 
  
  df <- filter(df,!!!filter_quos)
  
  attr(df,"data_cells") <- data_cells
  attr(df,"formats") <- formats
  
  df  
}

