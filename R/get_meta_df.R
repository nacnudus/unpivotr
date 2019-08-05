#' get metadata df
#'
#' This function:
#'          1. Identifies which cells are likely to to contains meta data (in top right corner)
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#' @param formats format object read in by `tidyxl::xlsx_cells`
#' @param col_groups format object read in by `tidyxl::xlsx_cells`
#'
#' @export


get_meta_groups <- function(sheet, value_ref, formats, .groupings = groupings(fmt_alignment_indent),
                        header_fill = "local_format_id", default_row_header_direction = default_row_header_direction,
                        table_data = tabledata, filter_headers_by = filter_headers_by_temp,
                        min_header_index = min_header_index_temp) {

  # Idenitfy header cells to which directions will be allocated
  
  # Get cells identified as col_headers 
  col_df <- sheet %>%
    filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
  
  col_df_crns <- col_df %>%
    filter(!is_blank) %>%
    get_corner_cell_refs()
  
  header_df <- sheet %>% filter(row < col_df_crns$max_row, col < col_df_crns$min_col)
  
  
  # Create additional row, col variables to allow for nesting
  
  header_df <-
    header_df %>% mutate(col_temp = col) %>% mutate(row_temp = row)
  
  # Check that at least one cell is in the header_df
  if (nrow(header_df) == 0) {
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
  
  header_df <- suppressMessages(fill_blanks_in_row_headers(header_df, header_fill, formats))
  
  
  # Create grouping variables for symbols provided to grouping.
  .groupings <- .groupings %>%
    append(quo(ones)) %>%
    append(quo(1 + 1))
  
  symbol_filter <- .groupings %>% map_lgl(~ type_of(get_expr(.x)) == "symbol")
  
  closures <- .groupings[symbol_filter]
  
  openenv <- environment()
  
  rm(list = ls()[str_detect(ls(), "^grp_")])
  
  seq_along(closures) %>%
    map(~ assign(paste0("grp_", as_label(closures[[.x]]) %>% str_remove_all("\\(\\)")),
                 set_env(eval_tidy(closures[[.x]])),
                 envir = openenv
    ))
  
  closure_list <- syms(ls()[str_detect(ls(), "grp_")])
  
  header_df <-
    header_df %>%
    mutate_at(
      .vars = "local_format_id",
      .funs = funs(!!!closure_list)
    )
  
  # Create grouping variables for symbols provided to grouping.
  
  fmt_forms <- closures <- .groupings[!symbol_filter]
  
  form_list <- fmt_forms %>% map(append_name_to_quosure)
  
  header_df <- append(list(header_df), form_list) %>% reduce(reduce_mutated)
  
  grouping_vars <- syms(names(header_df) %>% .[str_detect(., "^grp_")])
  
  
  # Nest meta groups
  header_df <-
    header_df %>%
    filter(coalesce(
      as.character(logical), as.character(numeric),
      as.character(date), as.character(character)
    ) != "") %>%
    group_by(col_temp,row_temp, !!!grouping_vars) %>%
    nest() %>%
    ungroup()
  
  # Name meta groups
  header_df <-
    header_df %>%
    mutate(row_no_name = row_number() + min_header_index + 1) %>%
    mutate(header_label = paste0("meta_header_label_", str_pad(row_no_name, 2, side = "left", "0")))
  
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
  
  
  # Set direction
  header_df <-
    header_df %>%
    mutate(direction = "WNW") %>% 
    dplyr::select(header_label, direction, data, !!!grouping_vars)
  
  
  # Add additional information
  header_df %>%
    mutate(data_summary = data %>% map(~ get_corner_cell_refs(.x))) %>%
    unnest(data_summary)
  
  header_vars <- syms(header_df$header_label)
  
  if (nrow(header_df) == 0) {
    return(header_df)
  }
  
  header_df <-
    header_df %>%
    unnest() %>%
    mutate(value = coalesce(!!!header_vars)) %>%
    select(row, col, .header_label = header_label, .direction = direction, .value = value)

  # Remove duplicated labels  
  header_df <- 
  header_df %>% group_by(.value,.header_label) %>% top_n(-1,wt = row_number())
  

}
