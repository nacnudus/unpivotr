
#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#'


fill_in_blanks <- function(sheet) {

  # Define blank cells
  blank_df <-
    sheet %>%
    filter(data_type == "blank")

  # Filter out blank cells - use this to check neighbours of blank cells
  joiner <- sheet %>% select(-character_formatted) %>% filter(!is_blank)

  # Check each cells agains the column to the left
  inserter <-
    blank_df %>%
    mutate(col_old = col, col = col - 1) %>%
    mutate(address_old = address) %>%
    select(sheet, row, col, col_old, local_format_id, address_old) %>%
    left_join(joiner) %>%
    mutate(address = address_old) %>%
    select(-address_old) %>%
    mutate(col = col_old) %>%
    select(-col_old) %>%
    filter(!is_blank) %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    mutate(merged = 1)



  # Join sheet with inserter
  sheet <-
    sheet %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    filter(!row_col %in% inserter$row_col) %>%
    bind_rows(inserter) %>%
    arrange(row, col)

  # Remove duplicates
  sheet %>% group_by(row, col) %>% top_n(n = 1, wt = row_number()) %>% ungroup()
}
