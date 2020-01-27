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
    
    # Default filter expression - numeric cells 
    if(length(filter_expresions) == 0){
      filter_expresions <- rlang::quos(!is.na(numeric))
    }
    
    # Add annotation variables if missing  
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    sheet <-  added_var_list %>% purrr::reduce(add_variable_if_missing)
   
    # Reorder variables  
    sheet <- 
      sheet %>% dplyr::select(.value, .direction, .header_label, dplyr::everything() )
    
    filter_expresions_type <- filter_expresions %>% purrr::map_chr(~typeof(rlang::get_expr(.x)))
    
    filter_expresions_string <- filter_expresions[filter_expresions_type == "string"]
    filter_expresions_langage <- filter_expresions[filter_expresions_type == "language"]
    filter_expresions_symbol <- filter_expresions[filter_expresions_type == "symbol"]
    
    openenv <- environment()
    
    # Limit to table Range ----
  
    if(length(filter_expresions_string) > 0){
      
        filter_expresions_string <- map(filter_expresions_string,string_expression_to_quosure)
        
        filter_quosures_string_names <- purrr::map(filter_expresions_string,rlang::get_expr)  %>% unlist() %>% 
          stringr::str_remove("\\:") %>% paste0("flt_",.) 
        
        names(filter_expresions_string) <- filter_quosures_string_names
      }
    
    if(length(filter_expresions_symbol) > 0){
      
      filter_expresions_symbol <-  map(filter_expresions_symbol, symbol_expression_to_quosure)
      
      filter_quosures_symbol_names <- 
        purrr::map(closures,rlang::as_label)  %>% unlist() %>%stringr::str_remove("\\:") %>% paste0("flt_",.) 
      
      names(filter_expresions_symbol) <- filter_quosures_symbol_names
    }
    
    if(length(filter_expresions_langage) > 0){
    
      filter_expresions_langage
  
      filter_quosures_language_names <- purrr::map_chr(filter_expresions_langage,name_quosure)
      
      names(filter_expresions_langage) <- filter_quosures_language_names
    
    }
    
   filter_quosures <- 
     list(filter_expresions_langage,filter_expresions_symbol,filter_expresions_string) %>% 
     purrr::reduce(append)

   if(length(filter_quosures) == 0) 
     stop("Error: Please provide an expression to `locate_data`")
   
    data_cell_filter <- 
      sheet %>% mutate(!!!filter_quosures) %>% dplyr::select(names(filter_quosures)) %>%    
      as.list %>% purrr::map(as.logical) %>% purrr::pmap_lgl(~ sum(...,na.rm = TRUE) > 0 )
    
    data_cells <- sheet[data_cell_filter,] 
    sheet <- sheet[!data_cell_filter,] 
    
    data_cells <- data_cells %>% 
      dplyr::mutate(.value = dplyr::coalesce(as.character(numeric),as.character(character),
                                             as.character(logical),as.character(date)))
    
    attr(sheet, "data_cells") <- data_cells 
    attr(sheet, "formats") <- format 
    
    class(sheet) <- append("located_data",class(sheet))
    
    sheet
    
  }

