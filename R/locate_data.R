#' Locates data cells 
#' @description
#' Removes data cells from data frame and stores in an attribute.  
#' @param sheet data frame produced by xlsx_cells
#' @param ... filter expression that identifies data cells.
#'
#' @export

locate_data <-
  function(sheet= NULL, ...) {
    format <-  attr(sheet, "formats")
    
    filter_expresions <- rlang::quos(...)
    
    if(length(filter_expresions) == 0){
      filter_expresions <- rlang::quos(!is.na(numeric))
    }
    
    # Add annotation variables if missing  
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    sheet <-  added_var_list %>% purrr::reduce(add_variable_if_missing)
    
    sheet <- 
      sheet %>% select(.value, .direction, .header_label, everything() )
    
    
    
    filter_expresions_type <- filter_expresions %>% purrr::map_chr(~type_of(rlang::get_expr(.x)))
    
    filter_expresions_string <- filter_expresions[filter_expresions_type == "string"]
    filter_expresions_langage <- filter_expresions[filter_expresions_type == "language"]
    filter_expresions_symbol <- filter_expresions[filter_expresions_type == "symbol"]
    
    # Limit to table Range ----
    filter_expresions_string <- filter_expresions_string %>% append(quo("A1"))  
    
    string_var_names <- map(filter_expresions_string,rlang::get_expr)  %>% unlist() %>% 
      stringr::str_remove("\\:") %>% paste0("flt_",.) %>%  rlang::syms()
    filter_expresions_list <-  list(sheet) %>% append(map(filter_expresions_string, rlang::get_expr))
    
    sheet <-  filter_expresions_list %>% purrr::reduce(string_range_to_filter_vars) 
    
    #---------------------------------------------------------------------------------------------
    
    closures <- filter_expresions_symbol  %>% append(rlang::quos(ones)) %>% append(rlang::quos(twos))
    
    openenv <- environment()
    
    seq_along(closures) %>% 
      map(~ assign(paste0("flt_", as_label(closures[[.x]]) %>% stringr::str_remove_all("\\(\\)") ),
                   set_env(eval_tidy(closures[[.x]])),envir = openenv))
    
    closure_list <- rlang::syms(ls()[stringr::str_detect(ls(),"flt_")]) 
    
    sheet <- 
      sheet %>% 
      dplyr::mutate_at(.vars = "local_format_id",
                .funs = funs(!!!closure_list))
    
    #------------------------------------------------------------------------------------------
    filter_expresions_langage <- filter_expresions_langage %>% append(quo(2 + 1))   
    
    fmt_forms <-  filter_expresions_langage     
    
    form_list <-  fmt_forms %>% map(append_name_to_quosure,prefix = "flt_")
    
    sheet <- append(list(sheet),form_list) %>% purrr::reduce(reduce_mutated)
    
    data_cell_filter <- 
      sheet %>% select(dplyr::starts_with("flt")) %>% dplyr::select(names(.)[!(names(.) %in% c("flt_X2_1", "flt_ones","flt_twos","flt_A1"))]) %>% 
      as.list %>% purrr::map(as.logical) %>% purrr::pmap_lgl(~ sum(...,na.rm = TRUE) > 0 )
    
    
    
    data_cells <- sheet[data_cell_filter,] %>% dplyr::select(-dplyr::starts_with("flt_"))
    
    sheet <- sheet[!data_cell_filter,] %>% dplyr::select(-dplyr::starts_with("flt_"))
    
    
    data_cells <- data_cells %>% 
      mutate(.value = dplyr::coalesce(as.character(numeric),as.character(character),
                               as.character(logical),as.character(date)))
    
    attr(sheet, "data_cells") <- data_cells 
    attr(sheet, "formats") <- format 
    
    sheet
    
  
  }

