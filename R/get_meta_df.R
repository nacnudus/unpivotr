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
  header_df <-
    sheet %>%
    filter(
      !is_blank,
      row <= max(col_groups$max_row),
      col < value_ref$min_col
    )


  # Get format information ----

  header_df <-
    header_df %>%
    mutate(col_temp = col) %>%
    mutate(row_temp = row)


  # Name columns
  header_df <-
    header_df %>%
    filter(!(is_blank & is.na(character))) %>%
    group_by(col_temp, row_temp) %>%
    nest() %>%
    ungroup()

  header_df <-
    header_df %>%
    mutate(row_no_name = row_temp - min(row_temp) + 1) %>%
    mutate(header_label = paste0(
      "header_meta_",
      str_pad(row_number(), 2, side = "left", "0")
    ))

  # Create and name headers ----
  header_df <-
    header_df %>%
    mutate(data = map2(
      data, header_label,
      function(data, header_label) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)
        temp_df[[header_label]] <- temp_df$value
        temp_df %>% select(-value)
      }
    ))



  # Set direction
  header_df <-
    header_df %>%
    mutate(direction = "WNW")


  # Add information to output df ----
  header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
}
