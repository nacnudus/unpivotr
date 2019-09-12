#' Conditionally adds direction annations to tidyxl data frame
#'
#' @description
#' This function conditionally adds direction annations to tidyxl data frame.
#'
#' @param numeric_value types
#' @param cells types
#' @param direction types
#' @param name types
#' @param values types
#' @param types types
#' @param formatters types
#' @param drop_na types
#' @export


migrate <- function(orientated_df, numeric_value = FALSE) {
  

  
  orientated_df_nested <-
    orientated_df %>%
    dplyr::filter(!is.na(.direction)) %>%
    dplyr::group_by(.direction, .header_label) %>%
    dplyr::mutate(value = dplyr::coalesce(character, as.character(numeric))) %>%
    dplyr::select(row, col, .value, .direction, .header_label) %>%
    tidyr::nest()

  header_dfs <- orientated_df_nested$data[orientated_df_nested$.direction != "data"]
  directions <- orientated_df_nested$.direction[orientated_df_nested$.direction != "data"]
  header_names <- orientated_df_nested$.header_label[orientated_df_nested$.direction != "data"]

  if (!is.null(attr(orientated_df, "data_cells"))) {
    data_cells <- attr(orientated_df, "data_cells") %>% dplyr::select(row, col, .value)
  } else {
    data_cells <- orientated_df_nested$data[orientated_df_nested$.direction == "data"][[1]]
  }


  header_dfs <-
    purrr::map2(header_dfs, header_names, function(header_df, header_name) {
      header_df %>%
        rename(!!rlang::ensym(header_name) := .value)
    })


  tidy_df <-
    list(
      x = header_dfs,
      y = directions
    ) %>%
    purrr::pmap(function(x, y) {
      enhead_tabledata(
        header_data = x, direction = y,
        values = data_cells
      )
    }) %>%
    purrr::reduce(dplyr::full_join, by = c("row", "col", ".value"))

  if (numeric_value) {
    tidy_df <-
      tidy_df %>%
      dplyr::mutate(.value = as.numeric(.value))
  }

  tidy_df
}
