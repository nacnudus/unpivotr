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
#' @examples

migrate <- function(orientated_df){

  orientated_df_nested <-
    orientated_df %>%
    group_by(.orientation, .header_group) %>%
    mutate(value = coalesce(character,as.character(numeric))) %>%
    select(row,col,value,.orientation,.header_group) %>%
    nest()

  data_row_index <- which(orientated_df_nested$.orientation == "data")
  header_dfs   <- orientated_df_nested$data[orientated_df_nested$.orientation != "data"]
  orientations <- orientated_df_nested$.orientation[orientated_df_nested$.orientation != "data"]
  header_names <- orientated_df_nested$.header_group[orientated_df_nested$.orientation != "data"]

  list(
    x = header_dfs,
    y = orientations,
    z = header_names) %>%
    pmap(function(x,y,z){
      enhead_tabledata(header_data = x, direction = y,header_label = z,
                       values = orientated_df_nested$data[[data_row_index]])} ) %>%
    reduce(full_join,by = c("row", "col", "value.data")) %>%
    rename(value = value.data)

}

#' A wrapper to enhead.
#'
#' @description
#' This function is used by [unpivotr::migrate] to create a tidy data frame using a `pmap` style workflow.
#'
#' @param header_data a list of data frames that are header subsets from the original data frame produced by tidyxl.
#' @param direction a vector of orientations
#' @param header_label a vector of names for variables associated with each header group.
#' @param values the central values of the table
#'
#' @name enhead_tabledata
#' @examples


enhead_tabledata <- function(header_data, direction,
                             header_label, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction) %>%
    rename(!!sym(header_label) := value.header)
}

