#' Transform a tidyxl data frame with orientations to a tidy data frame that has a column for each header label.
#'
#' @description
#' This function is to be used following [unpivotr::migrate()].
#' It transforms a tidyxl data frame with orientations to a tidy data frame that has a column for each header label.
#'
#' @param orientated_df  a tidyxl data frame with a `.orientation` and `.header.group` columns
#'
#' @name migrate
#' @export

migrate <- function(orientated_df){
  
  orientated_df_nested <-
    orientated_df %>%
    filter(!is.na(.orientation)) %>% 
    group_by(.orientation, .header_group) %>%
    mutate(value = coalesce(character,as.character(numeric))) %>%
    select(row,col,.value,.orientation,.header_group) %>%
    nest()
  
  data_row_index <- which(orientated_df_nested$.orientation == "data")
  header_dfs   <- orientated_df_nested$data[orientated_df_nested$.orientation != "data"]
  orientations <- orientated_df_nested$.orientation[orientated_df_nested$.orientation != "data"]
  header_names <- orientated_df_nested$.header_group[orientated_df_nested$.orientation != "data"]
  
  header_dfs <- 
    map2(header_dfs,header_names, function(header_df, header_name){
      header_df %>% 
        rename(!!sym(header_name) := .value)
    })
  
  list(
    x = header_dfs,
    y = orientations) %>%
    pmap(function(x,y){
      enhead_tabledata(header_data = x, direction = y,
                       values = orientated_df_nested$data[[data_row_index]])} ) %>%
    reduce(full_join,by = c("row", "col",".value"))  
  

  
}


#' A wrapper to enhead.
#'
#' @description
#' This function is used by [unpivotr::migrate] to create a tidy data frame using a `pmap` style workflow.
#'d
#' @param header_data a list of data frames that are header subsets from the original data frame produced by tidyxl.
#' @param direction a vector of orientations
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


