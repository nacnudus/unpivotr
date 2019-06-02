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
                           .groupings = groupings(fmt_alignment_indent), 
                           col_header_fill = "local_format_id",
                           default_col_header_direction = default_col_header_direction,
                           table_data = tabledata,
                           filter_col_headers_by = filter_col_headers_by_temp) {
  
 browser() 
  
 # Idenitfy header cells to which directions will be allocated  

  
  header_df <-
    sheet %>%
      filter(col <= value_ref$max_col) %>%
      filter(col >= value_ref$min_col) %>%
      filter(row < value_ref$min_row) 
  
 # Create additional row variables to allow for nesting  
  
  header_df <- 
    header_df %>% mutate(row_temp = row)
  
 # Check that at least one cell is in the header_df 
  
  if(nrow(header_df) == 0){
      warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
    
  header_df <- fill_blanks_in_headers(header_df, col_header_fill,formats)
  
 # create names for grouping functions  
  
  .groupings <- .groupings %>% append(enqou(1))
  
  types <- .groupings %>% map_chr(type_of)
  
  # Add formatting information from formulas ( ~ *)
    
  form_list <- seq_along(.groupings) %>% 
    map(~list(.groupings[.x],
              paste0("frm_",
                     .groupings[.x] %>% as.character() %>% make.names() %>% 
                       str_replace_all("\\.+",".") %>% str_remove_all("(\\.$)|(^X\\.)") %>% 
                       ifelse(str_sub(.,start = 1,1) %in% as.character(0:9),paste0("x",.),.  ))))
  
  
  form_list <- form_list %>% map(~.x %>% map(1))
  
  header_df <- append(list(header_df),form_list) %>% reduce(reduce_mutated)  
    
  grouping_vars <- syms(names(header_df) %>% .[str_detect(.,"cls_|frm_")])
  
  # Nest header groups ----
  header_df <-
    header_df %>% 
    filter(coalesce(as.character(logical),as.character(numeric),
                    as.character(date),as.character(character) ) != "") %>%  
    group_by(row_temp,!!!grouping_vars) %>% 
    filter(!(is_blank & is.na(character))) %>% 
    nest() %>%
    ungroup()
  
  # Name header groups
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
    dplyr::select(header_label, direction, data, !!!grouping_vars)
  

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
