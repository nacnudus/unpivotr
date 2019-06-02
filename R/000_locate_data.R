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
  function(sheet= NULL, 
           filter = NULL) {
    
    # Limit to table Range ----
    if (is.character(filter)) {
      
      cell_ref_df <- as_tibble(cellranger::as.cell_limits(table_range))
      
      table_range_df <-
        cell_ref_df[,1:2] %>%
        set_names(c("min","max")) %>%
        mutate(dimension = c("row","col")) %>%
        gather(key, value, -dimension) %>%
        unite(label, key, dimension, sep = "_") %>%
        spread(label, value )
      
      data_sheet <-
        sheet %>%
        filter(row >= table_range_df$min_row[1],
               row <= table_range_df$max_row[1],
               col >= table_range_df$min_col[1],
               col <= table_range_df$max_col[1])
      
    }
    
    
    
    if (purrr::is_formula(filter)) {
      
      current_quosure <-  as_quosure(filter)
      
     data_sheet <- 
        sheet %>% 
        filter(!!current_quosure)
      
    } 
    
     
    if (is.null(filter)) {
      
    
   value_ref <- 
     sheet %>% 
     dplyr::filter(!is.na(numeric)) %>%
     summarise(
       min_row = min(row), max_row = max(row),
       min_col = min(col), max_col = max(col)
     ) 
   
   data_sheet <-
    sheet %>%
    filter(row >= value_ref$min_row[1],
           row <= value_ref$max_row[1],
           col >= value_ref$min_col[1],
           col <= value_ref$max_col[1])
   
    } 
   
   sheet <-  
    sheet[!(sheet$address %in% data_sheet$address),]
     
    
    attr(sheet, "data_cells") <- data_sheet %>%  
      mutate(.value = coalesce(as.character(numeric),as.character(character),as.character(date),
                               as.character(logical),as.character(error), as.character(formula)))
    
    sheet
        
  }