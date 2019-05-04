#' Get Rowname DF
#'
#' This function:
#'          1. Identifies which cells are likely to be row names
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#' @param formats format object read in by `tidyxl::xlsx_cells`
#' @param col_groups format object read in by `tidyxl::xlsx_cells`
#' @param added_row_groups format object read in by `tidyxl::xlsx_cells`
#'
#' @export


get_row_groups <- function(sheet, value_ref, col_groups, formats, added_row_groups, 
                           group_row_headers_by = c("bolding","italics","indenting"), 
                           row_header_fill = "none", default_row_header_direction = "W",
                           table_data = tabledata ) {



  # Get row name cells
  header_df <-
    sheet %>%
    filter(
      row <= value_ref$max_row,
      row > max(col_groups$max_row),
      col < value_ref$min_col
    )
  
  if(nrow(header_df) == 0){
    warning("No row groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
    
    return(tibble())
  }
  
  
  # Fill in blanks ----
  
  if(row_header_fill ==  "style"){
    
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = FALSE)
      
      continue <- !identical(sheet_original, header_df)
    }
    
  }
  if(row_header_fill ==  "local_format_id"){
    
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = TRUE)
      
      continue <- !identical(sheet_original, header_df)
    }
    
  }
  if(row_header_fill ==  "borders"){
    

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
  

  # Get row name cell format information

  # 
  # if(group_row_headers_by[1] == "none"){
  #   
  #   format_funcs <- syms(c("ones","twos"))  
  #   
  # }else{
  #   format_funcs <- syms(c("ones","twos",group_row_headers_by))  
  #   
  # }
  
  

types <- group_row_headers_by %>% map_chr(type_of)
closures <- group_row_headers_by[types == "closure"]

openenv <- environment()

seq_along(closures) %>% 
  map(~ assign(paste0("closureno_", str_pad(as.character(.x),width = 2,side = "left",pad = 0)),
        closures[[.x]],envir = openenv))

closure_list <- syms(ls()[str_detect(ls(),"closureno_")])

header_df <- 
header_df %>% 
  mutate_at(.vars = "local_format_id",
            .funs = funs(!!!closure_list))


fmt_forms <- group_row_headers_by[types == "formula"]

form_list <- seq_along(fmt_forms) %>% 
  map(~list(fmt_forms[.x],
           paste0("formulano_", str_pad(as.character(.x),width = 2,side = "left",pad = 0)) ))

reduce_mutated <- function(df, form_list){
  
  current_quosure <-  as_quosure(form_list[[1]][[1]])
  var_name_sym <-  sym(form_list[[2]])
  
  df %>% 
    mutate(!!var_name_sym:= !!current_quosure)
} 

header_df <- append(list(header_df),form_list) %>% reduce(reduce_mutated)  


grouping_vars <- syms(names(header_df) %>% .[str_detect(.,"closureno_|formulano_|ones|twos")])


  # Nest row groups
  header_df <-
    header_df %>%
    mutate(col_temp = col) %>% 
    group_by(col_temp,!!!grouping_vars) %>%
    nest() %>%
    ungroup()
  
  # Name row groups
  header_df <-
    header_df %>%
    mutate(row_no_name = col_temp - min(col_temp) + 1) %>%
    mutate(row_group = paste0("row_label_", str_pad(row_number(), 2, side = "left", "0")))
  
  # Set row_group varnames and set values
  header_df <-
    header_df %>%
    mutate(data = map2(
      data, row_group,
      function(data, row_group) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)
        
        temp_df[[row_group]] <- temp_df$value
        
        temp_df %>% select(-value)
      }
    ))
  
  # check whether there are values in the rows
  
  
  
  empty_row_df <- 
  table_data %>% 
    select(-comment) %>% 
    group_by(row) %>% 
    summarise(empty_share = sum(value == "")/n())
  
  row_range <- c(min(empty_row_df$row):max(empty_row_df$row))
  
  missing_rows_from_headers <- row_range[!row_range %in% empty_row_df$row]
  
  empty_row_df <-
  bind_rows(empty_row_df,
            tibble(row = missing_rows_from_headers, 
                   empty_share = 1))
  
  # 
  # header_df <-
  #   header_df %>%
  #   mutate(row_sum = map_dbl(data, ~ get_row_sum(data = .x, sheet = sheet)))
  
  wnw_vector <- 
  header_df$data %>% map(~.$row)  %>% 
    map_lgl( ~ sum(.x %in% empty_row_df$row[empty_row_df$empty_share==1]) > 0)
    
    # Set directions
  header_df <-
    header_df %>%
    mutate(direction = ifelse(wnw_vector, "WNW", default_row_header_direction)) %>%
    dplyr::select(row_group, direction, data, !!!grouping_vars)



  # Add additional information
  header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
  
}

#' Get row sum
#'
#' This function is used to identify whether rows have a Wester or NNW orientation to data
#' @param data  a row_name_df object
#' @param sheet  a row_name_df object
#'
#' @export

get_row_sum <- function(data, sheet) {
  data %>%
    mutate(row_sum_values = map_dbl(
      row,
      function(x) {
        summarise(filter(sheet, row == x), filled = sum(numeric, na.rm = T))$filled
      }
    )) %>%
    summarise(row_sum_values = sum(row_sum_values, na.rm = T)) %>%
    pull(row_sum_values)
}
