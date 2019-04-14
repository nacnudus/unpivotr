#' get metadata df
#'
#' This function:
#'          1. Identifies which cells are likely to to contains meta data (in top right corner)
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#' @param formats format object read in by `tidyxl::xlsx_cells`
#' @param col_groups format object read in by `tidyxl::xlsx_cells`
#'
#' @export


get_meta_df <- function(sheet, value_ref, col_groups, formats) {

  ## Used for debugging
  # sheet <- master_df_01$sheet[[100]]
  # value_ref <- master_df_01$value_ref[[100]]
  # formats <- master_df_01$formats[[100]]
  # col_groups <- master_df_01$col_groups[[100]]
  # Get cells
  meta_df <-
    sheet %>%
    filter(
      !is_blank,
      row <= max(col_groups$max_row),
      col < value_ref$min_col
    )


  # Get cell format information
  meta_df <-
    meta_df %>%
    mutate(col_temp = col) %>%
    mutate(row_temp = row) %>%
    mutate(indent = local_format_id %>%
      map_int(possibly({
        ~ formats$local$alignment[["indent"]][[.x]]
      }, 0L)) %>%
      unlist()) %>%
    mutate(bold = local_format_id %>%
      map_lgl(possibly({
        ~ formats$local$font[["bold"]][[.x]]
      }, F)) %>%
      unlist()) %>%
    mutate(italic = local_format_id %>%
      map_lgl(possibly({
        ~ formats$local$font[["italic"]][[.x]]
      }, F)) %>%
      unlist())


  # Name columns
  meta_df <-
    meta_df %>%
    group_by(col_temp, row_temp, indent, bold, italic) %>%
    nest() %>%
    ungroup() %>%
    mutate(col_no_name = col_temp - min(col_temp) + 1) %>%
    mutate(row_no_name = row_temp - min(row_temp) + 1) %>%
    mutate(header_name = paste0(
      "row_", str_pad(row_no_name, 2, "left", "0"),
      "_col_", str_pad(col_no_name, 2, "left", "0"),
      "_in", indent,
      "_b", as.integer(bold),
      "_it", as.integer(italic)
    )) %>%
    mutate(meta_data = paste0("meta_data_", str_pad(row_number(), 2, side = "left", "0"))) %>%
    mutate(data = map2(
      data, meta_data,
      function(data, meta_data) {
        temp_df <- data %>% select(row, col, character)
        temp_df[[meta_data]] <- temp_df$character
        temp_df %>% select(-character)
      }
    ))


  # Set direction
  meta_df <-
    meta_df %>%
    mutate(direction = "WNW")

    # Add information
    meta_df %>%
    dplyr::select(meta_data, direction, data, indent, bold, italic) %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
}
