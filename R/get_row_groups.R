#' Get meta data groups
#'
#' This function:
#'          1. Identifies which cells are likely to be headers
#'          2. groups them according to their indenting, bold and italic formatting
#'          3. Specifies the unpivotr function specifying the direction of the header w.r.t. table data
#' Behead multiple header groups 
#' @description
#' Beheads multiple headers defined according to expressions in .groupings. 
#' @param sheet data frame created by xlsx_cells
#' @param value_ref  referece to where data cells are located. 
#' @param table_data datacell dataframe.
#' @param formats  format object created by tidyxl. 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_row_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param filter_headers_by method for dealing with merged cells.
#' @param min_header_index min header index

get_row_groups <- function(sheet, value_ref, formats, .groupings = groupings(fmt_alignment_indent),
                           header_fill = "local_format_id", default_row_header_direction = default_row_header_direction,
                           table_data = tabledata, filter_headers_by = filter_headers_by_temp,
                           min_header_index = min_header_index_temp) {
  # Idenitfy header cells to which directions will be allocated
  col_df <- sheet %>% dplyr::filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)

  col_df_crns <- col_df %>%
    dplyr::filter(!is_blank) %>%
    get_corner_cell_refs()

  header_df <- sheet %>% dplyr::filter(row <= value_ref$max_row, row > col_df_crns$max_row, col < value_ref$min_col)

  # Create additional row variables to allow for nesting

  header_df <- header_df %>% mutate(col_temp = col)

  # Check that at least one cell is in the header_df

  if (nrow(header_df) == 0) {
    warning("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }

  # Fill in blanks ----

  header_df <- suppressMessages(fill_blanks_in_row_headers(header_df, header_fill, formats))

  # Create grouping variables for symbols provided to grouping.
  .groupings <- .groupings %>%
    append(quo(ones)) %>%
    append(quo(1 + 1))

  symbol_filter <- .groupings %>% purrr::map_lgl(~ type_of(rlang::get_expr(.x)) == "symbol")

  closures <- .groupings[symbol_filter]

  openenv <- environment()

  rm(list = ls()[stringr::str_detect(ls(), "^grp_")])

  seq_along(closures) %>%
    map(~ assign(paste0("grp_", rlang::as_label(closures[[.x]]) %>% stringr::str_remove_all("\\(\\)")),
      set_env(eval_tidy(closures[[.x]])),
      envir = openenv
    ))

  closure_list <- rlang::syms(ls()[stringr::str_detect(ls(), "grp_")])

  header_df <-
    header_df %>%
    dplyr::mutate_at(
      .vars = "local_format_id",
      .funs = funs(!!!closure_list)
    )

  # Create grouping variables for symbols provided to grouping.

  fmt_forms <- closures <- .groupings[!symbol_filter]

  form_list <- fmt_forms %>% map(append_name_to_quosure)

  header_df <- append(list(header_df), form_list) %>% purrr::reduce(reduce_mutated)

  grouping_vars <- rlang::syms(names(header_df) %>% .[stringr::str_detect(., "^grp_")])


  # Nest row groups
  header_df <-
    header_df %>%
    dplyr::filter(dplyr::coalesce(
      as.character(logical), as.character(numeric),
      as.character(date), as.character(character)
    ) != "") %>%
    dplyr::group_by(col_temp, !!!grouping_vars) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  # Name row groups
  header_df <-
    header_df %>%
    mutate(row_no_name = dplyr::row_number() + min_header_index + 1) %>%
    mutate(header_label = paste0("row_header_label_", stringr::str_pad(row_no_name, 2, side = "left", "0")))

  # Set row_group varnames and set values
  header_df <-
    header_df %>%
    mutate(data = purrr::map2(
      data, header_label,
      function(data, header_label) {
        temp_df <- data %>%
          mutate(value = dplyr::coalesce(
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

  # check whether there are values in the rows

  empty_row_df <-
    table_data %>%
    mutate(value = dplyr::coalesce(
      as.character(numeric),
      as.character(character),
      as.character(logical),
      as.character(date)
    )) %>%
    select(-comment) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(empty_share = sum(value == "") / n()) %>%
    mutate(empty_share = ifelse(is.na(empty_share) | empty_share == 1, 1, 0))

  row_range <- c(min(empty_row_df$row):max(empty_row_df$row))

  missing_rows_from_headers <- row_range[!row_range %in% empty_row_df$row]

  empty_row_df <-
    dplyr::bind_rows(
      empty_row_df,
      tibble::tibble(
        row = missing_rows_from_headers,
        empty_share = 1
      )
    )

  #
  # header_df <-
  #   header_df %>%
  #   mutate(row_sum = purrr::map_dbl(data, ~ get_row_sum(data = .x, sheet = sheet)))

  wnw_vector <-
    header_df$data %>%
    map(~ .$row) %>%
    purrr::map_lgl(~ sum(.x %in% empty_row_df$row[empty_row_df$empty_share == 1]) > 0)

  if ("grp_fmt_alignment_indent" %in% names(header_df)) {
    wnw_vector <- wnw_vector | (header_df$grp_fmt_alignment_indent < max(header_df$grp_fmt_alignment_indent,na.rm = TRUE))
  }

  # Set directions
  header_df <-
    header_df %>%
    mutate(direction = ifelse(wnw_vector, "WNW", default_row_header_direction)) %>%
    dplyr::select(header_label, direction, data, !!!grouping_vars)

  # Add additional information
  header_df %>%
    mutate(data_summary = data %>% map(~ get_corner_cell_refs(.x))) %>%
    tidyr::unnest(data_summary)

  header_vars <- rlang::syms(header_df$header_label)

  if (nrow(header_df) == 0) {
    return(header_df)
  }

  header_df <-
    header_df %>%
    tidyr::unnest() %>%
    mutate(value = dplyr::coalesce(!!!header_vars)) %>%
    select(row, col, .header_label = header_label, .direction = direction, .value = value)

  header_df
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
    mutate(row_sum_values = purrr::map_dbl(
      row,
      function(x) {
        dplyr::summarise(dplyr::filter(sheet, row == x), filled = sum(numeric, na.rm = T))$filled
      }
    )) %>%
    dplyr::summarise(row_sum_values = sum(row_sum_values, na.rm = T)) %>%
    dplyr::pull(row_sum_values)
}
