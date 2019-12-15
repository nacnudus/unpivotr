#' Get meta data groups
#'
#' This function:
#'          1. Identifies which cells are likely to be headers
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' Behead multiple header groups 
#' @description
#' Beheads multiple headers defined according to expressions in .groupings. 
#' @param sheet data frame created by xlsx_cells
#' @param value_ref  reference to where data cells are located. 
#' @param table_data datacell dataframe.
#' @param formats  format object created by tidyxl. 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_row_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param filter_headers_by method for dealing with merged cells.
#' @param min_header_index min header index


get_meta_groups <- function(sheet, value_ref, formats, .groupings = groupings(fmt_alignment_indent),
                        header_fill = "local_format_id", default_row_header_direction = default_row_header_direction,
                        table_data = tabledata, filter_headers_by = filter_headers_by_temp,
                        min_header_index = min_header_index_temp) {

  # Idenitfy header cells to which directions will be allocated
  
  # Get cells identified as col_headers 
  col_df <- sheet %>%
    dplyr::filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
  
  col_df_crns <- col_df %>%
    dplyr::filter(!is_blank) %>%
    get_corner_cell_refs()
  
  header_df <- sheet %>% dplyr::filter(row < col_df_crns$max_row, col < col_df_crns$min_col)
  
  
  # Create additional row, col variables to allow for nesting
  
  header_df <-
    header_df %>% dplyr::mutate(col_temp = col) %>% dplyr::mutate(row_temp = row)
  
  # Check that at least one cell is in the header_df
  if (nrow(header_df) == 0) {
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
  
  header_df <- suppressMessages(fill_blanks_in_row_headers(header_df, header_fill, formats))
  
  
  # Create grouping variables for symbols provided to grouping.
  .groupings <- .groupings %>%
    append(rlang::quo(ones)) %>%
    append(rlang::quo(1 + 1))
  
  symbol_filter <- .groupings %>% purrr::map_lgl(~ typeof(get_expr(.x)) == "symbol")
  
  closures <- .groupings[symbol_filter]
  
  openenv <- environment()
  
  rm(list = ls()[stringr::str_detect(ls(), "^grp_")])
  
  seq_along(closures) %>%
    map(~ assign(paste0("grp_", rlang::as_label(closures[[.x]]) %>% stringr::str_remove_all("\\(\\)")),
                 rlang::set_env(rlang::eval_tidy(closures[[.x]])),
                 envir = openenv
    ))
  
  closure_list <- rlang::syms(ls()[stringr::str_detect(ls(), "grp_")])
  
  header_df <-
    header_df %>%
    dplyr::mutate_at(
      .vars = "local_format_id",
      .funs = tibble::lst(!!!closure_list)
    )
  
  # Create grouping variables for symbols provided to grouping.
  
  fmt_forms <- closures <- .groupings[!symbol_filter]
  
  form_list <- fmt_forms %>% map(append_name_to_quosure)
  
  header_df <- append(list(header_df), form_list) %>% purrr::reduce(reduce_mutated)
  
  grouping_vars <- rlang::syms(names(header_df) %>% .[stringr::str_detect(., "^grp_")])
  
  
  # Nest meta groups
  header_df <-
    header_df %>%
    dplyr::filter(dplyr::coalesce(
      as.character(logical), as.character(numeric),
      as.character(date), as.character(character)
    ) != "") %>%
    dplyr::group_by(col_temp,row_temp, !!!grouping_vars) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  
  # Name meta groups
  header_df <-
    header_df %>%
    dplyr::mutate(row_no_name = dplyr::row_number() + min_header_index + 1) %>%
    dplyr::mutate(header_label = paste0("meta_header_label_", stringr::str_pad(row_no_name, 2, side = "left", "0")))
  
  # Set row_group varnames and set values
  header_df <-
    header_df %>%
    dplyr::mutate(data = purrr::map2(
      data, header_label,
      function(data, header_label) {
        temp_df <- data %>%
          dplyr::mutate(value = dplyr::coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          dplyr::select(row, col, value)
        
        temp_df[[header_label]] <- temp_df$value
        
        temp_df %>% dplyr::select(-value)
      }
    ))
  
  
  # Set direction
  header_df <-
    header_df %>%
    dplyr::mutate(direction = "WNW") %>% 
    dplyr::select(header_label, direction, data, !!!grouping_vars)
  
  
  # Add additional information
  header_df %>%
    dplyr::mutate(data_summary = data %>% map(~ get_corner_cell_refs(.x))) %>%
    tidyr::unnest(data_summary)
  
  header_vars <- rlang::syms(header_df$header_label)
  
  if (nrow(header_df) == 0) {
    return(header_df)
  }
  
  header_df <-
    header_df %>%
    tidyr::unnest() %>%
    dplyr::mutate(value = dplyr::coalesce(!!!header_vars)) %>%
    dplyr::select(row, col, .header_label = header_label, .direction = direction, .value = value)

  # Remove duplicated labels  
  header_df <- 
  header_df %>% dplyr::group_by(.value,.header_label) %>% dplyr::top_n(-1,wt = dplyr::row_number())
  

}
