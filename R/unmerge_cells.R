
#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param strict_merging only merge on local_format_id.
#' @param merge_var the variable determining whether cells a merged.


unmerge_cells <- function(sheet, strict_merging = T, merge_var) {


  # Define blank cells
  blank_df <-
    sheet %>%
    dplyr::filter(data_type == "blank")
  
  
  
  # Filter out blank cells - use this to check neighbours of blank cells
  joiner <- sheet %>%
    dplyr::select(-character_formatted) %>%
    dplyr::filter(!is_blank)
  
  # Check each cells agains the column to the left
  
  if (strict_merging) {
    inserter <-
      blank_df %>%
      dplyr::mutate(col_old = col, col = col - 1) %>%
      dplyr::mutate(address_old = address) %>%
      dplyr::select(sheet, row, col, col_old, local_format_id, address_old) %>%
      dplyr::left_join(joiner, by = c("sheet", "row", "col", merge_var)) %>%
      dplyr::mutate(address = address_old) %>%
      dplyr::select(-address_old) %>%
      dplyr::mutate(col = col_old) %>%
      dplyr::select(-col_old) %>%
      dplyr::filter(!is_blank) %>%
      dplyr::mutate(row_col = paste0(row, "_", col)) %>%
      dplyr::mutate(merged = 1)
  } else {
    inserter <-
      blank_df %>%
      dplyr::mutate(col_old = col, col = col - 1) %>%
      dplyr::mutate(address_old = address) %>%
      dplyr::select(sheet, row, col, col_old, style_format, address_old) %>%
      dplyr::left_join(joiner,by = c("sheet", "row", "col", merge_var)) %>%
      dplyr::mutate(address = address_old) %>%
      dplyr::select(-address_old) %>%
      dplyr::mutate(col = col_old) %>%
      dplyr::select(-col_old) %>%
      dplyr::filter(!is_blank) %>%
      dplyr::mutate(row_col = paste0(row, "_", col)) %>%
      dplyr::mutate(merged = 1)
  }
  
  # Join sheet with inserter
  sheet <-
    sheet %>%
    dplyr::mutate(row_col = paste0(row, "_", col)) %>%
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


unmerge_row_cells <- function(sheet, strict_merging = T,merge_var) {
  
  # Define blank cells
  blank_df <-
    sheet %>%
    dplyr::filter(data_type == "blank")
  
  # Filter out blank cells - use this to check neighbours of blank cells
  joiner <- sheet %>%
    dplyr::select(-character_formatted) %>%
    dplyr::filter(!is_blank)
  
  # Check each cells agains the column to the left
  
  if (strict_merging) {
    inserter <-
      blank_df %>%
      dplyr::mutate(row_old = row, row = row - 1) %>%
      dplyr::mutate(address_old = address) %>%
      dplyr::select(sheet, row, col, row_old, local_format_id, address_old) %>%
      dplyr::left_join(joiner,by = c("sheet", "row", "col", merge_var)) %>%
      dplyr::mutate(address = address_old) %>%
      dplyr::select(-address_old) %>%
      dplyr::mutate(row = row_old) %>%
      dplyr::select(-row_old) %>%
      dplyr::filter(!is_blank) %>%
      dplyr::mutate(row_col = paste0(row, "_", col)) %>%
      dplyr::mutate(merged = 1)
  } else {
    inserter <-
      blank_df %>%
      dplyr::mutate(row_old = row, row = row - 1) %>%
      dplyr::mutate(address_old = address) %>%
      dplyr::select(sheet, row, col, row_old, style_format, address_old) %>%
      dplyr::left_join(joiner,by = c("sheet", "row", "col", merge_var)) %>%
      dplyr::mutate(address = address_old) %>%
      dplyr::select(-address_old) %>%
      dplyr::mutate(row = row_old) %>%
      dplyr::select(-row_old) %>%
      dplyr::filter(!is_blank) %>%
      dplyr::mutate(row_col = paste0(row, "_", col)) %>%
      dplyr::mutate(merged = 1)
  }
  
  # Join sheet with inserter
  sheet <-
    sheet %>%
    dplyr::mutate(row_col = paste0(row, "_", col)) %>%
    dplyr::filter(!row_col %in% inserter$row_col) %>%
    dplyr::bind_rows(inserter) %>%
    dplyr::arrange(row, col)
  
  # Remove duplicates
  sheet %>%
    dplyr::group_by(row, col) %>%
    dplyr::top_n(n = 1, wt = dplyr::row_number()) %>%
    dplyr::ungroup()
}
