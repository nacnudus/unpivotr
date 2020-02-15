#' Get column groups
#'
#' This is an internal function that:
#'          1. Identifies which cells are likely to be headers
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' Behead multiple header groups 
#' @description
#' Beheads multiple headers defined according to expressions in .groupings. 
#' @param sheet data frame created by xlsx_cells
#' @param direction  a string indicating which type of headers are to be labelled. Options include compass direction or up/down/left/right. 
#' @param value_ref  reference to where data cells are located. 
#' @param table_data datacell dataframe.
#' @param formats  format object created by tidyxl. 
#' @param .hook_if expression determining whether direction is hooked.
#' @param .hook_if_rev expression determining whether direction is reverse hooked.
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param min_header_index min header index

get_header_groups <- function(sheet, direction, value_ref, formats,
                              .groupings = groupings(fmt_alignment_indent),
                              .hook_if = hook(ones),
                              .hook_if_rev = hook(ones),
                              header_fill = "local_format_id",
                              default_col_header_direction = default_col_header_direction_temp,
                              table_data = tabledata,
                              min_header_index = min_header_index_temp) {
 
  # Allow grouings to take names 
  # Create a vector of names so that they aren't identified from all functions with regex  
  # Filter for header cells to which directions will be allocated based on direction --------------------------
  
  header_df <-  direction_filter_header(sheet,direction,value_ref)
  
  # Create additional row variables to allow for nesting
  
  header_df <- 
    header_df %>% 
    dplyr::mutate(rowcol_group = dplyr::case_when(
      direction %in% c("N", "S", "up","down") ~ row, 
      direction %in% c("W", "E", "left","right") ~ col))
  
  # Check that at least one cell is in the header_df
  
  if (nrow(header_df) == 0) {
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  # Fill in blanks ----
  
  header_df <- fill_blanks_in_headers(header_df, header_fill, formats, direction)
  
  #---------------------------------------------------------------------------------------------------
  # Create grouping variables for symbols provided to grouping.
  
  # Classify the filter expressions  
  grouping_expresions_type <- .groupings %>% purrr::map_chr(~typeof(rlang::get_expr(.x)))
  
  grouping_expresions_string <- .groupings[grouping_expresions_type == "string"]
  grouping_expresions_langage <- .groupings[grouping_expresions_type == "language"]
  grouping_expresions_symbol <- .groupings[grouping_expresions_type == "symbol"]
  
  # Give expressions names, and convert to quosures where expressions are symbols (fmt_ functions) or strings (spreadsheet ranges) 
  
  openenv <- environment()
  
  if(length(grouping_expresions_string) > 0){
    
    grouping_expresions_string <- grouping_expresions_string %>% string_expressions_to_quosures(environ = openenv)
    
    grouping_quosures_string_names <- grouping_expresions_string %>% name_string_expressions(prefix = "grp_")
    
    names(grouping_expresions_string) <- grouping_quosures_string_names
  }
  
  if(length(grouping_expresions_symbol) > 0){
    
    grouping_expresions_symbol_quo <-  grouping_expresions_symbol %>% symbol_expressions_to_quosures(environ = openenv)
    
    grouping_quosures_symbol_names <- grouping_expresions_symbol %>% name_symbol_expressions(prefix = "grp_")    
    
    names(grouping_expresions_symbol_quo) <- grouping_quosures_symbol_names
  }
  
  if(length(grouping_expresions_langage) > 0){
    
    grouping_quosures_language_names <- grouping_expresions_langage %>% name_language_expressions(prefix = "grp_")
    
    names(grouping_expresions_langage) <- grouping_quosures_language_names
    
  }
 
  # Combine grouping quosures 
  grouping_quosures <- 
    list(
      if(exists("grouping_expresions_langage")){grouping_expresions_langage},
      if(exists("grouping_expresions_symbol_quo")){grouping_expresions_symbol_quo},
      if(exists("grouping_expresions_string")){grouping_expresions_string}) %>% 
    purrr::reduce(append)
  
  grouping_quosures <- grouping_quosures[!is.null(grouping_quosures)]
  
  format <- formats
  # Convert grouping vars (flt_) to a lgl vector
  header_df <- header_df %>% mutate(!!!grouping_quosures)  

  #--------------------------------------------------------------------------------------------
  
  # Nest header groups ----
  header_df <-
    header_df %>%
    dplyr::filter(dplyr::coalesce(as.character(logical), as.character(numeric),
                                  as.character(date), as.character(character)) != "") %>%
    dplyr::group_by(rowcol_group, !!!grouping_quosures) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  
  # Name header groups
  header_df <-
    header_df %>%
    dplyr::mutate(row_no_name = dplyr::row_number() + min_header_index - 1) %>%
    dplyr::mutate(header_label = paste0(direction,"_header_label_", stringr::str_pad(row_no_name, 2, side = "left", "0")))
  
  
  
  # Get directions --------------------------------------------------------------------------------------
  
  
  names(.hook_if) <- "hook_var"
  names(.hook_if_rev) <- "hook_var_rev"
  
  header_df_hook <- 
    header_df %>% 
    tidyr::unnest(cols = data) %>%  
    dplyr::group_by(header_label) %>%
    dplyr::summarise(!!!.hook_if)
  
  header_df_hook_rev <- 
    header_df %>% 
    tidyr::unnest(cols = data) %>%  
    dplyr::group_by(header_label) %>%
    dplyr::summarise(!!!.hook_if_rev)
  
  header_df <- 
    dplyr::full_join(header_df_hook,header_df_hook_rev,by = "header_label") %>% 
    dplyr::left_join(header_df,.,by = "header_label")
  
  # Create additional row variables to allow for nesting
  
  hook_direction <- dplyr::case_when(
    direction %in% c("N","up") ~ "NNW",
    direction %in% c("W","left") ~ "WNW",
    direction %in% c("S","down") ~ "SSW",
    direction %in% c("E","right") ~ "ENE")
  
  hook_direction_rev <- dplyr::case_when(
    direction %in% c("N","up") ~ "NNE",
    direction %in% c("W","left") ~ "WSW",
    direction %in% c("S","down") ~ "SSE",
    direction %in% c("E","right") ~ "ESE")
  
  
  # Set direction ----
  header_df <-
    header_df %>%
    dplyr::mutate(direction = dplyr::case_when(
      hook_var_rev == TRUE ~ hook_direction_rev, 
      hook_var == TRUE     ~ hook_direction, 
      TRUE                 ~ direction)) %>%
    dplyr::select(header_label, direction, data, names(grouping_quosures))
  
  # remove extra variables ----
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
  
  
  # Add information to output df ----
  
  header_df <-
    header_df %>%
    dplyr::mutate(data_summary = data %>%
                    purrr::map(~ .x %>% dplyr::summarise(
                      min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
                      min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
                    ))) %>%
    tidyr::unnest(data_summary)
  
  header_vars <- rlang::syms(header_df$header_label)
  
  if (nrow(header_df) == 0) {
    
    return(header_df)
  }
  
  
  header_df <-
    header_df %>%
    tidyr::unnest(cols = data) %>%
    dplyr::mutate(value = dplyr::coalesce(!!!header_vars)) %>%
    dplyr::select(row, col, .header_label = header_label, .direction = direction, .value = value)
  
  header_df
  
  # %>%
  # check_low_col_names
}