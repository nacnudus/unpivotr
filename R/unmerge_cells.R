
#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param strict_merging only merge on local_format_id.


unmerge_cells <- function(sheet, strict_merging = T) {


  # Define blank cells
  blank_df <-
    sheet %>%
    dplyr::filter(data_type == "blank")



  # Filter out blank cells - use this to check neighbours of blank cells
  joiner <- sheet %>%
    select(-character_formatted) %>%
    dplyr::filter(!is_blank)

  # Check each cells agains the column to the left

  if (strict_merging) {
    inserter <-
      blank_df %>%
      mutate(col_old = col, col = col - 1) %>%
      mutate(address_old = address) %>%
      select(sheet, row, col, col_old, local_format_id, address_old) %>%
      dplyr::left_join(joiner) %>%
      mutate(address = address_old) %>%
      select(-address_old) %>%
      mutate(col = col_old) %>%
      select(-col_old) %>%
      dplyr::filter(!is_blank) %>%
      mutate(row_col = paste0(row, "_", col)) %>%
      mutate(merged = 1)
  } else {
    inserter <-
      blank_df %>%
      mutate(col_old = col, col = col - 1) %>%
      mutate(address_old = address) %>%
      select(sheet, row, col, col_old, style_format, address_old) %>%
      dplyr::left_join(joiner) %>%
      mutate(address = address_old) %>%
      select(-address_old) %>%
      mutate(col = col_old) %>%
      select(-col_old) %>%
      dplyr::filter(!is_blank) %>%
      mutate(row_col = paste0(row, "_", col)) %>%
      mutate(merged = 1)
  }

  # Join sheet with inserter
  sheet <-
    sheet %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    dplyr::filter(!row_col %in% inserter$row_col) %>%
    dplyr::bind_rows(inserter) %>%
    dplyr::arrange(row, col)

  # Remove duplicates
  sheet %>%
    dplyr::group_by(row, col) %>%
    dplyr::top_n(n = 1, wt = dplyr::row_number()) %>%
    dplyr::ungroup()
}


# Rows


unmerge_row_cells <- function(sheet, strict_merging = T) {

  # Define blank cells
  blank_df <-
    sheet %>%
    dplyr::filter(data_type == "blank")

  # Filter out blank cells - use this to check neighbours of blank cells
  joiner <- sheet %>%
    select(-character_formatted) %>%
    dplyr::filter(!is_blank)

  # Check each cells agains the column to the left

  if (strict_merging) {
    inserter <-
      blank_df %>%
      mutate(row_old = row, row = row - 1) %>%
      mutate(address_old = address) %>%
      select(sheet, row, col, row_old, local_format_id, address_old) %>%
      dplyr::left_join(joiner) %>%
      mutate(address = address_old) %>%
      select(-address_old) %>%
      mutate(row = row_old) %>%
      select(-row_old) %>%
      dplyr::filter(!is_blank) %>%
      mutate(row_col = paste0(row, "_", col)) %>%
      mutate(merged = 1)
  } else {
    inserter <-
      blank_df %>%
      mutate(row_old = row, row = row - 1) %>%
      mutate(address_old = address) %>%
      select(sheet, row, col, row_old, style_format, address_old) %>%
      dplyr::left_join(joiner) %>%
      mutate(address = address_old) %>%
      select(-address_old) %>%
      mutate(row = row_old) %>%
      select(-row_old) %>%
      dplyr::filter(!is_blank) %>%
      mutate(row_col = paste0(row, "_", col)) %>%
      mutate(merged = 1)
  }

  # Join sheet with inserter
  sheet <-
    sheet %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    dplyr::filter(!row_col %in% inserter$row_col) %>%
    dplyr::bind_rows(inserter) %>%
    dplyr::arrange(row, col)

  # Remove duplicates
  sheet %>%
    dplyr::group_by(row, col) %>%
    dplyr::top_n(n = 1, wt = dplyr::row_number()) %>%
    dplyr::ungroup()
}
