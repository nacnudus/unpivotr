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


get_row_groups <- function(sheet, value_ref, formats,.groupings = groupings(fmt_alignment_indent), 
                           header_fill = "local_format_id",default_row_header_direction = default_row_header_direction,
                           table_data = tabledata, filter_headers_by = filter_headers_by_temp){

  # Idenitfy header cells to which directions will be allocated  
  
  col_df <-  sheet %>% filter(col <= value_ref$max_col,col >= value_ref$min_col,row < value_ref$min_row) 
  
  header_df <- sheet %>% filter(row <= value_ref$max_row,row > max(col_df$max_row),col < value_ref$min_col)
  
  # Create additional row variables to allow for nesting  
  
  header_df <- header_df %>% mutate(col_temp = col)
  
  # Check that at least one cell is in the header_df 
  
  if(nrow(header_df) == 0){
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
  
  header_df <- fill_blanks_in_row_headers(header_df, header_fill,formats) 
  
  # Create grouping variables for symbols provided to grouping.  
  .groupings <- .groupings %>% append(quo(ones)) %>%  append(quo(1+ 1))
  
  symbol_filter <- .groupings %>% map_lgl(~ type_of(get_expr(.x)) ==  "symbol")
  
  closures <- .groupings[symbol_filter]  
  
  openenv <- environment()
  
  rm(list = ls()[str_detect(ls(),"^grp_")])
  
  seq_along(closures) %>% 
    map(~ assign(paste0("grp_", as_label(closures[[.x]]) %>% str_remove_all("\\(\\)") ),
                 set_env(eval_tidy(closures[[.x]])),envir = openenv))
  
  closure_list <- syms(ls()[str_detect(ls(),"grp_")])
  
  header_df <- 
    header_df %>% 
    mutate_at(.vars = "local_format_id",
              .funs = funs(!!!closure_list))  
  
  # Create grouping variables for symbols provided to grouping.    
  
  fmt_forms <-   closures <- .groupings[!symbol_filter]  
  
  form_list <-  fmt_forms %>% map(append_name_to_quosure)
  
  header_df <- append(list(header_df),form_list) %>% reduce(reduce_mutated)
  
  grouping_vars <- syms(names(header_df) %>% .[str_detect(.,"^grp_")])

  # Nest row groups
  header_df <-
    header_df %>%
    filter(coalesce(as.character(logical),as.character(numeric),
                    as.character(date),as.character(character) ) != "") %>%  
    group_by(col_temp,!!!grouping_vars) %>%
    nest() %>%
    ungroup()
  
  # Name row groups
  header_df <-
    header_df %>%
    mutate(row_no_name = col_temp - min(col_temp) + 1) %>%
    mutate(header_label = paste0("row_label_", str_pad(row_number(), 2, side = "left", "0")))
  
  # Set row_group varnames and set values
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
  
  # check whether there are values in the rows
  
  empty_row_df <- 
    table_data %>%
    mutate(value = coalesce(
      as.character(numeric),
      as.character(character),
      as.character(logical),
      as.character(date)
    )) %>% 
    select(-comment) %>% 
    group_by(row) %>% 
    summarise(empty_share = sum(value == "")/n()) %>% 
    mutate(empty_share = ifelse(is.na(empty_share)|empty_share== 1,1,0))
  
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
    dplyr::select(header_label, direction, data, !!!grouping_vars)

  # Add additional information
  header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
  
  header_vars <- syms(header_df$header_label)
  
  header_df <- 
    header_df %>% 
    unnest() %>% 
    mutate(value = coalesce(!!!header_vars)) %>% 
    select(row,col,.header_label = header_label, .direction = direction, .value = value)
  
  header_df
  
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
