#' Locates data cells 
#' @description
#' Removes data cells from data frame and stores in an attribute.  
#' @param sheet data frame produced by xlsx_cells
#' @param ... filter expression that identifies data cells.
#'
#' @export
#' @examples print("todo")

locate_data <-
  function(sheet= NULL, ...) {
    
    
    
    format <-  attr(sheet, "formats")
    
    filter_expresions <- rlang::quos(...)
    
    # Default filter expression - numeric cells 
    if(length(filter_expresions) == 0){
      print("No expression provided. Applying default filter: !is.na(numeric)")
      filter_expresions <- rlang::quos(!is.na(numeric))
    }
    
    # Add annotation variables if missing  
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    sheet <-  added_var_list %>% purrr::reduce(add_variable_if_missing)
   
    # Reorder variables  
    sheet <- 
      sheet %>% dplyr::select(.value, .direction, .header_label, dplyr::everything() )
   
    # Classify the filter expressions  
    filter_expresions_type <- filter_expresions %>% purrr::map_chr(~typeof(rlang::get_expr(.x)))
    
    filter_expresions_string <- filter_expresions[filter_expresions_type == "string"]
    filter_expresions_langage <- filter_expresions[filter_expresions_type == "language"]
    filter_expresions_symbol <- filter_expresions[filter_expresions_type == "symbol"]
    
    # Give expressions names, and convert to quosures where expressions are symbols (fmt_ functions) or strings (spreadsheet ranges) 
  
    openenv <- environment()
     
    if(length(filter_expresions_string) > 0){
      
        filter_expresions_string <- filter_expresions_string %>% string_expressions_to_quosures(environ = openenv)
        
        filter_quosures_string_names <- filter_expresions_string %>% name_string_expressions(prefix = "flt_")
        
        names(filter_expresions_string) <- filter_quosures_string_names
      }
    
    if(length(filter_expresions_symbol) > 0){
      
      filter_expresions_symbol <-  filter_expresions_symbol %>% symbol_expressions_to_quosures(environ = openenv)
    
      filter_quosures_symbol_names <- filter_expresions_symbol %>% name_symbol_expressions(prefix = "flt_")    
    
        names(filter_expresions_symbol) <- filter_quosures_symbol_names
    }
    
    if(length(filter_expresions_langage) > 0){
    
      filter_expresions_langage
  
      filter_quosures_language_names <- filter_expresions_langage %>% name_language_expressions(prefix = "flt_")
      
      names(filter_expresions_langage) <- filter_quosures_language_names
    
    }
    
    # Combine filter quosures 
    filter_quosures <- 
     list(filter_expresions_langage,filter_expresions_symbol,filter_expresions_string) %>% 
     purrr::reduce(append)

   # Convert filter vars (flt_) to a lgl vector
    data_cell_filter <- 
      sheet %>% mutate(!!!filter_quosures) %>% # Create flt_ vars 
      dplyr::select(names(filter_quosures)) %>% # filter for flt_ vars     
      as.list %>% purrr::map(as.logical) %>%   # Convert flt_ vars to logical 
      purrr::pmap_lgl( ~ sum(...,na.rm = TRUE) > 0) # Combine lgls from flt_ vars 
    
    # Filter out data cells 
    data_cells <- sheet[data_cell_filter,] 
    sheet <- sheet[!data_cell_filter,] 
    
    # Create .value var  
    data_cells <- data_cells %>% 
      dplyr::mutate(.value = dplyr::coalesce(as.character(numeric),as.character(character),
                                             as.character(logical),as.character(date)))
    
    # Add attributes 
    attr(sheet, "data_cells") <- data_cells 
    attr(sheet, "formats") <- format 
    
    # Add class 
    class(sheet) <- append("located_data",class(sheet))
    
    sheet
    
  }

