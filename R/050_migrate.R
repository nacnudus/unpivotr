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

migrate <- function(orientated_df, numeric_value = FALSE){
  
  orientated_df_nested <-
    orientated_df %>%
    filter(!is.na(.direction)) %>% 
    group_by(.direction, .header_label) %>%
    mutate(value = coalesce(character,as.character(numeric))) %>%
    select(row,col,.value,.direction,.header_label) %>%
    nest()
  
  data_row_index <- which(orientated_df_nested$.direction == "data")
  header_dfs   <- orientated_df_nested$data[orientated_df_nested$.direction != "data"]
  directions <- orientated_df_nested$.direction[orientated_df_nested$.direction != "data"]
  header_names <- orientated_df_nested$.header_label[orientated_df_nested$.direction != "data"]
  data_cells <- attr(orientated_df, "data_cells") %>% select(row,col,.value)

  
  
  header_dfs <- 
    map2(header_dfs,header_names, function(header_df, header_name){
      header_df %>% 
        rename(!!sym(header_name) := .value)
    })
  
  
  tidy_df <- 
  list(
    x = header_dfs,
    y = directions) %>%
    pmap(function(x,y){
      enhead_tabledata(header_data = x, direction = y,
                       values = data_cells)} ) %>%
    reduce(full_join,by = c("row", "col",".value"))  
  
  if(numeric_value){
    tidy_df <- 
    tidy_df %>% 
      mutate(.value = as.numeric(.value))
    
  }
  
  tidy_df
  
  
  
}


#' A wrapper to enhead.
#'
#' @description
#' This function is used by [unpivotr::migrate] to create a tidy data frame using a `pmap` style workflow.
#'d
#' @param header_data a list of data frames that are header subsets from the original data frame produced by tidyxl.
#' @param direction a vector of directions
#' @param header_label a vector of names for variables associated with each header group.
#' @param values the central values of the table
#'
#' @name enhead_tabledata


enhead_tabledata <- function(header_data, direction, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction) 
}


