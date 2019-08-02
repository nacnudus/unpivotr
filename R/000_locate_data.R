#' get tidyABS components
#'
#' Moves data cells to an attribute of the tidyxl data frame. 
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#' @param filter condition idnetifying cells.
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export

locate_data <-
  function(sheet= NULL, ... = !is.na(numeric)) {
    
    format <-  attr(sheet, "formats")
    
    filter_expresions <- quos(...)
    
    filter_expresions_type <- filter_expresions %>% map_chr(~type_of(get_expr(.x)))
    
    filter_expresions_string <- filter_expresions[filter_expresions_type == "string"]
    filter_expresions_langage <- filter_expresions[filter_expresions_type == "language"]
    filter_expresions_symbol <- filter_expresions[filter_expresions_type == "symbol"]

    # Limit to table Range ----
    filter_expresions_string <- filter_expresions_string %>% append(quo("A1"))  
    
    string_var_names <- map(filter_expresions_string,get_expr)  %>% unlist() %>% 
        str_remove("\\:") %>% paste0("flt_",.) %>%  syms()
      filter_expresions_list <-  list(sheet) %>% append(map(filter_expresions_string, get_expr))
    
      sheet <-  filter_expresions_list %>% reduce(string_range_to_filter_vars) 
    
    #---------------------------------------------------------------------------------------------
    
    closures <- filter_expresions_symbol  %>% append(quos(ones)) %>% append(quos(twos))
    
    openenv <- environment()
    
    seq_along(closures) %>% 
      map(~ assign(paste0("flt_", as_label(closures[[.x]]) %>% str_remove_all("\\(\\)") ),
                   set_env(eval_tidy(closures[[.x]])),envir = openenv))
    
    closure_list <- syms(ls()[str_detect(ls(),"flt_")]) 
    
    sheet <- 
    sheet %>% 
      mutate_at(.vars = "local_format_id",
                .funs = funs(!!!closure_list))
    
    #------------------------------------------------------------------------------------------
    filter_expresions_langage <- filter_expresions_langage %>% append(quo(2 + 1))   
    
    fmt_forms <-  filter_expresions_langage     
    
    form_list <-  fmt_forms %>% map(append_name_to_quosure,prefix = "flt_")
    
    sheet <- append(list(sheet),form_list) %>% reduce(reduce_mutated)
        
  data_cell_filter <- 
    sheet %>% select(starts_with("flt")) %>% select(names(.)[!(names(.) %in% c("flt_X2_1", "flt_ones","flt_twos","flt_A1"))]) %>% 
    as_list %>% map(as.logical) %>% pmap_lgl(~ sum(...,na.rm = TRUE) > 0 )
  
  data_cells <- sheet[data_cell_filter,] %>% dplyr::select(-starts_with("flt_"))
    
  sheet <- sheet[!data_cell_filter,] %>% dplyr::select(-starts_with("flt_"))
    
  data_cells <- data_cells %>% 
    mutate(.value = coalesce(as.character(numeric),as.character(character),
                             as.character(logical),as.character(date)))
    
  attr(sheet, "data_cells") <- data_cells 
  attr(sheet, "formats") <- format 
  
  sheet
        
  }

