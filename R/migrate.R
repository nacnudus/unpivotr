#' Reshape a tidyxl data frame such that each header group has its own column and each data cells has its own row.
#'
#' @description
#' This function is used on a tidy data frame that has been annotated with `locate_data`, `locate_header` or  `locate_header_groups`. It creates a column for each header group. 
#'
#' @param located_df a tidyxl data frame with .direction, .value, .header_label columns.
#' @export
#' @examples print("todo")


migrate <- function(located_df) {
  
  orientated_df_nested <-
    located_df %>%
    dplyr::filter(!is.na(.direction)) %>%
    dplyr::group_by(.direction, .header_label) %>%
    dplyr::mutate(value = dplyr::coalesce(character, as.character(numeric))) %>%
    dplyr::select(row, col, .value, .direction, .header_label) %>%
    tidyr::nest()

  header_dfs <- orientated_df_nested$data[orientated_df_nested$.direction != "data"]
  directions <- orientated_df_nested$.direction[orientated_df_nested$.direction != "data"]
  header_names <- orientated_df_nested$.header_label[orientated_df_nested$.direction != "data"]
  
  if (!is.null(attr(located_df, "data_cells"))) {
    data_cells <- attr(located_df, "data_cells") %>% dplyr::select(row, col, .value)
  } else {
    data_cells <- orientated_df_nested$data[orientated_df_nested$.direction == "data"][[1]]
  }
  
  
  header_dfs <-
    purrr::map2(header_dfs, header_names, function(header_df, header_name) {
      header_df %>%
        dplyr::rename(!!rlang::ensym(header_name) := .value)
    })
  
  
  tidy_df <-
    list(
      x = header_dfs,
      y = directions
    ) %>%
    purrr::pmap(function(x, y) {
      enhead(data_cells =data_cells,header_cells =x,direction = y)
    }) %>%
    purrr::reduce(dplyr::full_join, by = c("row", "col", ".value"))
  
  
  tidy_df
}
