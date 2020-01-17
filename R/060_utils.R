#' paste3
#'
#' An internal fucntion that removes NAs from paste.
#' Source: https://stackoverflow.com/users/1855677/42.
#'
#' @param ... objects to be pasted together.
#' @param sep separating string.

paste3 <- function(..., sep = ", ") {
  L <- list(...)
  L <- lapply(L, function(x) {
    x[is.na(x)] <- ""
    x
  })
  ret <- gsub(
    paste0("(^", sep, "|", sep, "$)"), "",
    gsub(
      paste0(sep, sep), sep,
      do.call(paste, c(L, list(sep = sep)))
    )
  )
  is.na(ret) <- ret == ""
  ret
}

#' Supply an expression to identify which header groups are hooked.
#'
#' @description
#' This function is used with the `.hook_if` or `.hook_if_rev` arguments in the `locate_groups` function.
#' It passes an expression to `dplyr::summarise` that identifies which header groups are hooked - for example swiched from N to NNW.
#' See the the `locate_groups` documentation for more information and an example.
#'
#' @param ...  expression applied to a  identifies which header groups are hooked
#'
#' @name get_range_dfs
#' @export

hook <- function(...) {
  rlang::quos(...)
}

#' Unmerge and fill in blanks for row headers.
#'
#' This is an internal function that function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells.
#' @param header_fill the criteria according to which blank cells will be filled.
#' @param formats the formats of the workbook associated with the tidyxl data frame.

fill_blanks_in_row_headers <- function(header_df, header_fill, formats) {
  if (header_fill == "style") {
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = FALSE,merge_var = "style_format")
      
      continue <- !identical(sheet_original, header_df)
    }
  }
  
  if (header_fill == "local_format_id") {
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_row_cells(strict_merging = TRUE,merge_var = "local_format_id")
      
      continue <- !identical(sheet_original, header_df)
    }
  }
  
  if (header_fill == "borders") {
    filled_join <-
      header_df %>%
      add_v_border_groups(formats) %>%
      dplyr::group_by(v_border_group) %>%
      dplyr::select(row, col, v_border_group, character) %>%
      dplyr::mutate(value = ifelse(is.na(character), paste3(character, collapse = " _ ") %>% stringr::str_remove_all(" _ "), character)) %>%
      dplyr::ungroup() %>%
      dplyr::select(row, col, character = value)
    
    header_df <-
      header_df %>%
      dplyr::select(-character) %>%
      dplyr::left_join(filled_join, by = c("row", "col")) %>%
      dplyr::arrange(row, col)
  }
  
  header_df
}

#' Unmerge and fill in blanks for col headers.
#'
#' This is an internal function that function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells.
#' @param header_fill the criteria according to which blank cells will be filled.
#' @param formats the formats of the workbook associated with the tidyxl data frame.

fill_blanks_in_col_headers <- function(header_df, header_fill, formats) {
  if (header_fill == "style") {
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = FALSE, merge_var = "style_format")
      
      continue <- !identical(sheet_original, header_df)
    }
  }
  if (header_fill == "local_format_id") {
    continue <- TRUE
    
    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = TRUE, merge_var = "local_format_id")
      
      continue <- !identical(sheet_original, header_df)
    }
  }
  
  if (header_fill == "borders") {
    filled_join <-
      header_df %>%
      add_h_border_groups(formats) %>%
      dplyr::group_by(h_border_group) %>%
      dplyr::select(row, col, h_border_group, character) %>%
      dplyr::mutate(value = ifelse(is.na(character), paste3(character, collapse = " _ ") %>%
                                     stringr::str_remove_all(" _ "), character)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(h_border_group, row, col) %>%
      dplyr::select(row, col, character = value)
    
    header_df <-
      header_df %>%
      dplyr::select(-character) %>%
      dplyr::left_join(filled_join, by = c("row", "col"))
  }
  
  header_df
}



#' Unmerge and fill in blanks for headers
#'
#' This is an internal function that function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells.
#' @param header_fill he criteria according to which blank cells will be filled.
#' @param formats the formats of the workbook associated with the tidyxl data frame.
#' @param direction the direction in which headers are filled.

fill_blanks_in_headers <- function(header_df, header_fill, formats, direction) {
  if (direction %in% c("N", "S", "up", "down")) {
    header_df <- fill_blanks_in_col_headers(header_df, header_fill, formats)
  } else if (direction %in% c("W", "E", "left", "right")) {
    header_df <- fill_blanks_in_row_headers(header_df, header_fill, formats)
  } else {
    stop("Please check the direction you are providing")
  }

  header_df
}

#' Add a set of variables based on a list of formulae
#'
#' This is an internal function that takes a data frame and a list of formulae and appends an arbitrary number of columns to the data frame based on the formulae.
#'
#' @param df a data frame containing header cells.
#' @param form_list list of formulae
#' @param format the formats associated with the workbook containing the header_df cells.
#'

reduce_mutated <- function(df, form_list, format) {
  current_quosure <- form_list[[1]]
  var_name_sym <- rlang::sym(form_list[[2]])

  df %>%
    dplyr::mutate(!!var_name_sym := !!current_quosure)
}

#' Give quosure a name
#'
#' This is an internal function that adds a prefixed name to a quosure so that variables added to a data frame using this quosure have a predictable name.
#' @param x quosure
#' @param prefix prefix to be added in name
#'

append_name_to_quosure <- function(x, prefix = "grp_") {
  list(
    x,
    paste0(
      prefix,
      x %>% rlang::as_label() %>% make.names() %>%
        stringr::str_replace_all("\\.+", ".") %>% stringr::str_remove_all("(\\.$)|(^X\\.)") %>%
        stringr::str_replace_all("\\.", "_") %>%
        ifelse(stringr::str_sub(., start = 1, 1) %in% as.character(0:9), paste0("x", .), .)
    )
  )
}

#' Add a variable to a tidyxl data frame if that variable is missing
#'
#' This is an internal function that takes a tidyxl data frame and a variable name to be added if they are missing.
#' @param sheet sheet nominated for tidying
#' @param var name of the var to be added as a character string.
#'
add_variable_if_missing <- function(sheet, var) {
  if (!var %in% names(sheet)) {
    var_sym <- rlang::sym(var)

    sheet <- bind_rows(sheet , tibble(!!var_sym := character()))
  }

  sheet
}

#' Create a filtering variable based on a excel string range.
#'
#' This is an internal function that create a variabled prefixed "flt" based on whether cells are within a range specified by an excel string range.
#' @param table_range a table_range given by string. For example, "A1:B2"
#' @param sheet a tidyxl data frame.

string_range_to_filter_vars <- function(sheet, table_range) {
  cell_ref_df <- tibble::as_tibble(cellranger::as.cell_limits(table_range))

  table_range_df <-
    cell_ref_df[, 1:2] %>%
    purrr::set_names(c("min", "max")) %>%
    dplyr::mutate(dimension = c("row", "col")) %>%
    tidyr::gather(key, value, -dimension) %>%
    tidyr::unite(label, key, dimension, sep = "_") %>%
    tidyr::spread(label, value)

  string_filter_name <- rlang::sym(table_range %>% stringr::str_remove("\\:") %>% paste0("flt_", .))

  data_sheet <-
    sheet %>%
    dplyr::mutate(!!string_filter_name :=
      row >= table_range_df$min_row[1] &
        row <= table_range_df$max_row[1] &
        col >= table_range_df$min_col[1] &
        col <= table_range_df$max_col[1])
}

#' Get the corner references of a tidyxl data frame.
#'
#' This is an internal function that takes a tidyxl data frame a returns a dataframe indicating the maximum and minimum corners.
#'
#' @param sheet a tidyxl data frame.

get_corner_cell_refs <- function(sheet) {

  list(min_row = min(sheet$row),
       max_row = max(sheet$row),
       min_col = min(sheet$col),
       max_col = max(sheet$col)
  )
}

#' This is an internal function to wrap `enhead`. It takes header cells, data cells and a direction and combines them into a tidy data frame.
#'
#' @param header_data a data frame containing header cells.
#' @param direction direction suplied to enhead.
#' @param values the data values.

enhead_tabledata <- function(header_data, direction, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction
  )
}

#' The the current names of header groups.
#'
#' This is an internal function that returns the names of header groups. 
#' Because header group names are produced automatically (for example, W_header_label_01) and are indexed, 
#' this function is required to determine what header names are already used
#' in the annotated tidyxl data frame. 
#'
#' @param labels list of labels.
#' @param regex_term prefix for col_header.

get_header_index <- function(labels, regex_term = "^col_header") {

current_index_numbers <- 
# Take a vector of header labels.
    labels %>%
# Extract those that look like automatically generated header names.
    .[stringr::str_detect(., regex_term)] %>%
# Extract index.
    stringr::str_extract("\\d")%>%
    as.numeric() 
  
if(length(current_index_numbers[!is.na(current_index_numbers)]) == 0 ){
  return(0)
}else{

current_index_numbers %>% 
  # Get highest index.
  max(., na.rm = TRUE) %>%
    # Deal with cases that have no index
    ifelse(is.finite(.), ., 0) %>%
    # increment on index.
    `+`(1)
  
}
  
}

#' Matches directions to unicode arrows for an interative chart
#'
#' A data frame with .direction, .arrow and .rate columns.
#' Because rotation of unicode arrows is not possible with plotly, 
#' a separate set of straight arrows is used for interative use of plot_cells.
#' 
#' @format a dataframe
"direction_plot_interactive"

#' Matches directions to unicode arrows for an interative chart
#'
#' A data frame with .direction, .arrow and .rate columns
#' Since ggplot allows for rotation of plotted text, 
#' this datafrom is comprised of right-angled arrows, 
#' some of which are rotated by plot cells.
#'
#' @format a dataframe
"direction_plot_noninteractive"


#' Data frame used for testing locate_data()
#'
#' Data frame used for testing locate_data()
#'
#' @format a dataframe
"locate_data_test"


#' Data frame used for testing locate
#'
#' Data frame used for testing locate()
#'
#' @format a dataframe
"locate_test"


#' Data frame used for testing locate_groups()
#'
#' Data frame used for testing locate_groups()
#'
#' @format a dataframe
"locate_groups_test"


#' Data frame used for testing migrate()
#'
#' Data frame used for testing migrate()
#'
#' @format a dataframe
"migrate_test"
