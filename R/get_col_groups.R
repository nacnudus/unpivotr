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
                           header_fill = "local_format_id",
                           default_col_header_direction = default_col_header_direction_temp,
                           table_data = tabledata,
                           filter_headers_by = filter_headers_by_temp,
                           min_header_index = min_header_index_temp) {
  # Idenitfy header cells to which directions will be allocated
  header_df <- sheet %>%
    filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
  
  # Create additional row variables to allow for nesting
  
  header_df <- header_df %>% mutate(row_temp = row)
  
  # Check that at least one cell is in the header_df
  
  if (nrow(header_df) == 0) {
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
  
  header_df <- suppressMessages(fill_blanks_in_col_headers(header_df, header_fill, formats))
  
  # Create grouping variables for symbols provided to grouping.
  
  .groupings <- .groupings %>%
    append(quo(ones)) %>%
    append(quo(1 + 1))
  
  symbol_filter <- .groupings %>% map_lgl(~ type_of(get_expr(.x)) == "symbol")
  
  closures <- .groupings[symbol_filter]
  
  openenv <- environment()
  
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
  
  # Nest header groups ----
  header_df <-
    header_df %>%
    filter(coalesce(
      as.character(logical), as.character(numeric),
      as.character(date), as.character(character)
    ) != "") %>%
    group_by(row_temp, !!!grouping_vars) %>%
    nest() %>%
    ungroup()
  
  # Name header groups
  header_df <-
    header_df %>%
    mutate(row_no_name = row_number() + min_header_index - 1) %>%
    mutate(header_label = paste0("col_header_label_", str_pad(row_no_name, 2, side = "left", "0")))
  
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

  header_df$data

  # Add information to output df ----

  header_df <-
    header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
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

  header_df

  # %>%
  # check_low_col_names
}
