#' Create horizontal border groups
#'
#' An internal fucntion that identifies groups based on borders of header cells.
#'
#' @param df a tidyxl data frame.
#' @param formats the formats of the sheet's workbook.

create_h_border_groups <- function(df, formats) {
  borders_df <-
    df %>%
    dplyr::mutate(right_border = local_format_id %>% purrr::map_lgl(~ formats$local$border$right$style[[.x]] != "")) %>%
    dplyr::mutate(left_border = local_format_id %>% purrr::map_lgl(~ formats$local$border$left$style[[.x]] != "")) %>%
    dplyr::select(address, row_temp, col, left_border, right_border)

  left_borders <- borders_df$col[borders_df$left_border] %>%
    .[!is.na(.)] %>%
    unique()
  right_borders <- borders_df$col[borders_df$right_border] %>%
    .[!is.na(.)] %>%
    unique()
  
  right_border <- c(right_borders, left_borders - 1) %>%
    unique() %>%
    sort()
  
  current_row <- df$row_temp %>% unique()
  
  borders_df <-
    tibble::tibble(max_col = right_border) %>%
    dplyr::mutate(min_col = dplyr::lag(max_col, 1) + 1) %>%
    dplyr::mutate(min_col = ifelse(dplyr::row_number() == 1, 0, min_col)) %>%
    dplyr::bind_rows(., tibble::tibble(max_col = 1000, min_col = max(.$max_col)) + 1) %>%
    dplyr::mutate(h_border_group = paste0(current_row, "_", as.character(dplyr::row_number()))) %>%
    dplyr::mutate(col = purrr::map2(min_col, max_col, function(x, y) {
      c(x:y)
    }))
  
  border_join <-
    borders_df %>%
    dplyr::select(h_border_group, col) %>%
    tidyr::unnest()
  
  df %>%
    dplyr::left_join(border_join, by = "col") %>%
    dplyr::select(row = row_temp, col, h_border_group)
}


#' Create horizontal border group variable
#'
#' An internal fucntion that adds a variable delineating cell groups based on horizontal borders of header cells.
#'
#' @param sheet a tidyxl data frame.
#' @param formats the formats of the sheet's workbook.

add_h_border_groups <- function(sheet, formats) {
  sheet_01 <-
    sheet %>%
    dplyr::mutate(row_temp = row) %>%
    dplyr::group_by(row) %>%
    tidyr::nest()
  
  df <- sheet_01$data[[1]]
  
  bgs <- sheet_01$data %>%
    purrr:map(create_h_border_groups, formats = formats) %>%
    dplyr::bind_rows()
  
  sheet %>% dplyr::left_join(bgs, by = c("row", "col"))
}


#' Create vertical border group variable
#'
#' An internal fucntion that adds a variable delineating cell groups based on borders of header cells.
#'
#' @param sheet a tidyxl data frame.
#' @param formats the formats of the sheet's workbook.


add_v_border_groups <- function(sheet, formats) {
  sheet_01 <-
    sheet %>%
    dplyr::mutate(col_temp = col) %>%
    dplyr::group_by(col) %>%
    tidyr::nest()
  
  df <- sheet_01$data[[1]]
  
  bgs <- sheet_01$data %>%
    purrr::map(create_v_border_groups, formats = formats) %>%
    dplyr::bind_rows()
  
  sheet %>% dplyr::left_join(bgs, by = c("row", "col"))
}


#' Create vertical border groups
#'
#' An internal fucntion that identifies groups based on vertical borders of header cells.
#'
#' @param df a tidyxl data frame.
#' @param formats the formats of the sheet's workbook.


create_v_border_groups <- function(df, formats) {
  borders_df <-
    df %>%
    dplyr::mutate(top_border = local_format_id %>% purrr::map_lgl(~ formats$local$border$top$style[[.x]] != "")) %>%
    dplyr::mutate(bottom_border = local_format_id %>% purrr::map_lgl(~ formats$local$border$bottom$style[[.x]] != "")) %>%
    dplyr::select(address, col_temp, row, top_border, bottom_border)
  
  top_borders <- borders_df$row[borders_df$top_border] %>%
    .[!is.na(.)] %>%
    unique()
  bottom_borders <- borders_df$row[borders_df$bottom_border] %>%
    .[!is.na(.)] %>%
    unique()
  
  bottom_borders
  
  border <- c(top_borders, bottom_borders + 1) %>%
    unique() %>%
    sort()
  
  current_col <- df$col_temp %>% unique()
  
  borders_df <-
    tibble::tibble(min_row = border) %>%
    dplyr::mutate(max_row = dplyr::lead(min_row, 1) - 1) %>%
    dplyr::mutate(max_row = ifelse(dplyr::row_number() == nrow(.), 1000, max_row)) %>%
    dplyr::bind_rows(tibble::tibble(max_row = min(.$min_row) - 1, min_row = 0), .) %>%
    dplyr::mutate(v_border_group = paste0(current_col, "_", as.character(dplyr::row_number()))) %>%
    dplyr::mutate(row = purrr::map2(min_row, max_row, function(x, y) {
      c(x:y)
    }))
  
  border_join <-
    borders_df %>%
    dplyr::select(v_border_group, row) %>%
    tidyr::unnest()
  
  df %>%
    dplyr::left_join(border_join, by = "row") %>%
    dplyr::select(col = col_temp, row, v_border_group)
}
