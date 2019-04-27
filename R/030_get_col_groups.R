#' Get column groups
#'
#' This function:
#'          1. Identifies which cells are likely to be headers
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#' @param formats format object read in by `tidyxl::xlsx_cells`
#'
#' @export


get_col_groups <- function(sheet, value_ref, formats, 
                           group_col_headers_by = c("bolding"), 
                           col_header_fill = "local_format_id",
                           default_col_header_direction = default_col_header_direction ) {
  
  
  # Get header cells ----

  header_df <-
    sheet %>%
    filter(col <= value_ref$max_col) %>%
    filter(col >= value_ref$min_col) %>%
    filter(row < value_ref$min_row) %>%
    mutate(row_temp = row)
  
  if(nrow(header_df) == 0){
      warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
    
  if(col_header_fill ==  "style"){
    
     continue <- TRUE

    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = FALSE)

      continue <- !identical(sheet_original, header_df)
    }
    
  }
  if(col_header_fill ==  "local_format_id"){
    
    continue <- TRUE

    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = TRUE)

      continue <- !identical(sheet_original, header_df)
    }
    
  }
  

  if(col_header_fill ==  "borders"){
  
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
  
  
  # Get format information ----
  
  if(group_col_headers_by[1] == "none"){
    
    format_funcs <- syms(c("ones","twos"))  
    
  }else{
    format_funcs <- syms(c("ones","twos",group_col_headers_by))  
    
  }

  header_df <- 
    header_df %>% 
    mutate_at(.vars = "local_format_id",
              .funs = funs(!!!format_funcs))    
    
  
  
  # Nest header groups ----
  header_df <-
    header_df %>% 
    group_by(row_temp,!!!format_funcs) %>% 
    filter(!(is_blank & is.na(character))) %>% 
    nest() %>%
    ungroup()
  

  header_df <-
    header_df %>%
    mutate(row_no_name = row_temp - min(row_temp) + 1) %>%
    mutate(header_label = paste0("header_label_", 
                                 str_pad(row_number(), 2, side = "left", "0")))
  
  # Create and name headers ----
  header_df <-
    header_df %>%
    mutate(data = map2(
      data, header_label,
      function(data, header_label) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)
        temp_df[[header_label]] <- temp_df$value
        temp_df %>% select(-value)
      }
    ))
  
  # Set direction ----
  header_df <-
    header_df %>%
    mutate(direction = default_col_header_direction) %>%
    dplyr::select(header_label, direction, data, !!!format_funcs)
  

  # Add information to output df ----
  header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary) 
  
  # %>%
  # check_low_col_names
}
