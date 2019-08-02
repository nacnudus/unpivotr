#' Transform a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @description
#' This function is to be used following [unpivotr::migrate()].
#' It transforms a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @param orientated_df  a tidyxl data frame with a `.direction` and `.header.group` columns
#'
#' @name migrate
#' @export


locate_dc <- function(cells, direction, name, values = NULL, types = data_type,
                   formatters = list(), drop_na = TRUE){
  
  # Store attributes 
  if(!is.null(attr(cells,"data_cells"))){
    data_cells_attr <- attr(cells,"data_cells")
  }
  
  if(!is.null(attr(cells, "formats"))){
    format <-  attr(cells, "formats")
  }
  

  # Add annotation variables  
  added_var_list <- list(cells,".header_label",".direction", ".value")
  
  cells <-  added_var_list %>% reduce(add_variable_if_missing)
  
  cells %>% select(row,col,character) %>% spread(col,character)
  
  cells_f <- cells %>% filter(is.na(.direction)) %>% filter(!is.na(character) | !is.na(numeric)) 
  
  
  
  
  data_cells$dc <- 1 
  
  direction_temp  = direction 
  name_temp = sym(name) 
  values_temp = values 
  types_temp = quo(types)
  formatters_temp = formatters 
  drop_na_temp = drop_na
  
  temp_df <- 
    bind_rows(cells,data_cells) %>%   
    locate(direction = direction_temp, name = !!name_temp) 
  
  data_cells <- temp_df %>% filter(dc == 1) %>% select(-dc)
  
  cells <- temp_df %>% filter(is.na(dc)) %>% select(-dc) 
  
  attr(cells,"data_cells") <- data_cells 
  attr(cells,"formats") <- formats 
  
  cells  
  
} 