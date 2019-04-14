#' Get Rowname DF
#'
#' This function:
#'          1. Identifies which cells are likely to be row names
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param value_ref data frame representing corners of numeric cells in excel sheet
#' @param formats format object read in by `tidyxl::xlsx_cells`
#' @param col_groups format object read in by `tidyxl::xlsx_cells`
#' @param added_row_groups format object read in by `tidyxl::xlsx_cells`
#'
#' @export


get_row_groups <- function(sheet, value_ref, col_groups, formats, added_row_groups) {


  # Get row name cells
  row_name_df <-
    sheet %>%
    filter(
      !is_blank,
      row <= value_ref$max_row,
      row > max(col_groups$max_row),
      col < value_ref$min_col
    )

  if(nrow(row_name_df) == 0){
    stop("No row groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }




  # Get row name cell format information
  row_name_df <-
    row_name_df %>%
    mutate(col_temp = col) %>%
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

  # Add manually identified row groups
  if (!is.null(added_row_groups)) {
    added_row_df <-
      tibble(address = added_row_groups) %>%
      mutate(added_group_no = row_number()) %>%
      unnest()

    row_name_df <-
      row_name_df %>%
      left_join(added_row_df) %>%
      mutate_at(
        .vars = vars(indent, bold, italic),
        .funs = funs(ifelse(is.na(added_group_no), ., NA))
      )
  } else {
    row_name_df <-
      row_name_df %>%
      mutate(added_group_no = NA)
  }

  # Nest row groups
  row_name_df <-
    row_name_df %>%
    group_by(col_temp, indent, bold, italic, added_group_no) %>%
    nest() %>%
    ungroup()

  # Name row groups
  row_name_df <-
    row_name_df %>%
    arrange(col_temp, indent, italic, bold) %>%
    mutate(row_group = paste0("row_group_", str_pad(row_number(), 2, side = "left", "0")))

  # Set row_group varnames and set values
  row_name_df <-
    row_name_df %>%
    mutate(data = map2(
      data, row_group,
      function(data, row_group) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)

        temp_df[[row_group]] <- temp_df$value

        temp_df %>% select(-value)
      }
    ))

  # check whether there are values in the rows
  row_name_df <-
    row_name_df %>%
    mutate(row_sum = map_dbl(data, ~ get_row_sum(data = .x, sheet = sheet)))

  # Set directions
  row_name_df <-
    row_name_df %>%
    mutate(direction = ifelse(row_sum == 0, "WNW", "W")) %>%
    dplyr::select(row_group, direction, data, indent, bold, italic, added_group_no)



  # Add additional information
  row_name_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
}


#' Get row sum
#'
#' This function is used to identify whether rows have a Wester or NNW orientation to data
#' @param data  a row_name_df object
#' @param sheet  a row_name_df object
#'
#' @export

get_row_sum <- function(data, sheet) {
  data %>%
    mutate(row_sum_values = map_dbl(
      row,
      function(x) {
        summarise(filter(sheet, row == x), filled = sum(numeric, na.rm = T))$filled
      }
    )) %>%
    summarise(row_sum_values = sum(row_sum_values, na.rm = T)) %>%
    pull(row_sum_values)
}
