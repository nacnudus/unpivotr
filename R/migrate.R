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

migrate <- function(orientated_df, numeric_value = FALSE) {
  orientated_df_nested <-
    orientated_df %>%
    filter(!is.na(.direction)) %>%
    group_by(.direction, .header_label) %>%
    mutate(value = coalesce(character, as.character(numeric))) %>%
    select(row, col, .value, .direction, .header_label) %>%
    nest()

  header_dfs <- orientated_df_nested$data[orientated_df_nested$.direction != "data"]
  directions <- orientated_df_nested$.direction[orientated_df_nested$.direction != "data"]
  header_names <- orientated_df_nested$.header_label[orientated_df_nested$.direction != "data"]

  if (!is.null(attr(orientated_df, "data_cells"))) {
    data_cells <- attr(orientated_df, "data_cells") %>% select(row, col, .value)
  } else {
    data_cells <- orientated_df_nested$data[orientated_df_nested$.direction == "data"][[1]]
  }


  header_dfs <-
    map2(header_dfs, header_names, function(header_df, header_name) {
      header_df %>%
        rename(!!sym(header_name) := .value)
    })


  tidy_df <-
    list(
      x = header_dfs,
      y = directions
    ) %>%
    pmap(function(x, y) {
      enhead_tabledata(
        header_data = x, direction = y,
        values = data_cells
      )
    }) %>%
    reduce(full_join, by = c("row", "col", ".value"))

  if (numeric_value) {
    tidy_df <-
      tidy_df %>%
      mutate(.value = as.numeric(.value))
  }

  tidy_df
}
