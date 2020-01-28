#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom methods is
#' @importFrom utils View installed.packages

# Spurious imports to satisfy R CMD check
#' @importFrom purrr map

NULL

globalVariables(c(
  ".",
  "inner_join",
  "mutate",
  "select",
  "rename",
  "quo",
  "UQ",
  "quo_name",
  "from_row",
  "from_col",
  "to_row",
  "to_col",
  "type",
  "value",
  "everything",
  "data_type",
  "is_na",
  ".value",
  ".data_type",
  "n",
  ":=",
  ".partition",
  "ns_env",
  "corner_row",
  "corner_col",
  ".boundary",
  ".arrow",
  ".direction", 
  ".direction.n", 
  ".direction.o", 
  ".header_label",
  ".header_label.n", 
  ".header_label.o", 
  ".value.n", 
  ".value.o", 
  "address", 
  "address_old",
  "bottom_border",
  "col_old", 
  "col_temp",
  "data",
  "data_summary",
  "dc",
  "default_col_header_direction_temp",
  "dimension",
  "direction",
  "empty_share",
  "filter_headers_by_temp",
  "formats",
  "funs",
  "h_border_group",
  "locate_header_groups_if",
  "max_col",
  "max_row",
  "min_col",
  "min_header_index_temp",
  "min_row",
  "row_col",
  "row_no_name",
  "row_old",
  "row_sum_values",
  "row_temp",
  "style_format",
  "tabledata",
  "v_border_group",
  "top_border",
  "local_format_id",
  "header_label",
  "is_blank",
  "label",
  "key",
  "character_formatted",
  "header_label",
  "is_blank",
  "left_border",
  "values",
  "error", 
  "purrr", 
  "sheet",
  "rowcol_group",
  ".rotate"
  
))

# Concatenate lists into vectors, handling factors and NULLs, and coercing data
# types only when necessary
concatenate <- function(..., combine_factors = TRUE, fill_factor_na = TRUE) {
  c.POSIXct <- function(..., recursive = FALSE) {
    .POSIXct(c(unlist(lapply(list(...), unclass))), tz = "UTC")
  }
  dots <- (...)
  dots_is_null <- purrr::map_lgl(dots, rlang::is_null)
  # If all elements are NULL, return as-is
  if (all(dots_is_null)) {
    return(dots)
  }
  # If any non-NULL elements aren't scalars, return as-is
  dots_is_scalar_vector <- purrr::map_lgl(dots, rlang::is_scalar_vector)
  if (any(!dots_is_scalar_vector[!dots_is_null])) {
    return(dots)
  }
  classes <- purrr::map(dots, class)
  # It might be safe to use c() if all non-NA/NULLs are the same class.
  if (length(unique(classes[!dots_is_null])) == 1L) {
    # The first element of each class is the telling one
    all_classes <- classes[!dots_is_null][[1]]
    first_class <- all_classes[1]
    # If it's a factor, then forcats::fct_c() could combine the levels if so
    # desired.
    if (first_class %in% c("factor", "ordered")) {
      # If combining_factors then forcats::fct_c() needs all elements to be
      # factors, so replace them each with an NA factor. Or even if you're not
      # combining factors but still want some kind of consistency.
      if (combine_factors || fill_factor_na) {
        dots[dots_is_null] <- list(factor(NA_character_))
      }
      if (combine_factors) {
        return(forcats::fct_c(rlang::splice(dots)))
      }
      else {
        return(dots)
      }
    } else {
      # c() omits NULLs, so replace them with NA, which c() will promote when
      # necessary.
      dots[dots_is_null] <- NA
      dots <- do.call(c, dots)
      # c() demotes dates etc. when the first element is NA, so replace the
      # classes.
      class(dots) <- all_classes
      return(dots)
    }
  }
  # Here, not every non-NA/NULL element is the same class, and c() isn't very
  # clever about homogenising things, so handle factors and dates manually.
  # c() ignores nulls, so replace them with NA.
  dots[dots_is_null] <- NA
  # Convert factors to strings before they're (potentially) coerced to integers
  factors <- purrr::map_lgl(classes, ~ .[1] %in% c("factor", "ordered"))
  dots[factors] <- purrr::map(dots[factors], as.character)
  # Convert dates to strings before they're (potentially) coerced to numbers
  dates <- purrr::map_lgl(classes, ~ .[1] %in% c("Date", "POSIXct", "POSIXlt"))
  dots[dates] <- purrr::map(dots[dates], format, justify = "none", trim = TRUE)
  # Finally go with c()'s default homegnising of remaining classes.  Don't use
  # purrr::flatten(), because it strips classes from dates.
  do.call(c, dots)
}

# Return an NA of the same type as the given vector
na_types <- list(
  logical = NA,
  integer = NA_integer_,
  double = NA_real_,
  character = NA_character_,
  complex = NA_complex_
)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))

# Apply custom functions to list-elements of a list-column created by pack()
# whose type matches the custom function.
maybe_format_list_element <- function(x, name, functions) {
  func <- functions[[name]]
  if (is.null(func)) func <- identity
  func(x)
}

# Standardise dialects of directions
standardise_direction <- function(direction) {
  stopifnot(length(direction) == 1L)
  dictionary <-
    c(`up-left` = "up-left", `up` = "up", `up-right` = "up-right",
      `right-up` = "right-up", `right` = "right", `right-down` = "right-down",
      `down-right` = "down-right", `down` = "down", `down-left` = "down-left",
      `left-down` = "left-down", `left` = "left", `left-up` = "left-up",
      `up-ish` = "up-ish", `right-ish` = "right-ish",
      `down-ish` = "down-ish", `left-ish` = "left-ish",
      NNW = "up-left", N = "up", NNE = "up-right",
      ENE = "right-up", E = "right", ESE = "right-down",
      SSE = "down-right", S = "down", SSW = "down-left",
      WSW = "left-down", W = "left", WNW = "left-up",
      ABOVE = "up-ish", RIGHT = "right-ish",
      BELOW = "down-ish", LEFT = "left-ish")
  if (direction %in% names(dictionary)) return(unname(dictionary[direction]))
  stop("The direction \"", direction, "\" is not recognised.  See ?directions.")
}


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


#' Unmerge and fill in blanks for row headers.
#'
#' This is an internal function that function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells.
#' @param header_fill the criteria according to which blank cells will be filled.
#' @param formats the formats of the workbook associated with the tidyxl data frame.

fill_blanks_in_row_headers <- function(header_df, header_fill = c("style", "local_format_id", "borders"), formats) {
  
  header_fill = match.arg(header_fill)
  
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

fill_blanks_in_col_headers <- function(header_df, header_fill = c("style", "local_format_id", "borders"), formats) {
  
  header_fill = match.arg(header_fill)
  
  
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

fill_blanks_in_headers <- function(header_df, header_fill = c("style", "local_format_id", "borders"), formats, direction) {
 
  header_fill = match.arg(header_fill)
  
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



#' Add a variable to a tidyxl data frame if that variable is missing
#'
#' This is an internal function that takes a tidyxl data frame and a variable name to be added if they are missing.
#' @param sheet sheet nominated for tidying
#' @param var name of the var to be added as a character string.
#'
add_variable_if_missing <- function(sheet, var) {
  if (!var %in% names(sheet)) {
    var_sym <- rlang::sym(var)
    
    sheet <- dplyr::bind_rows(sheet , tibble::tibble(!!var_sym := character()))
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
    return(1)
    
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


#' Get path to example data
#' Unpivotr comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
unpivotr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "unpivotr")) %>% .[stringr::str_detect(., "xlsx$")]
  } else {
    system.file("extdata", path, package = "unpivotr", mustWork = TRUE)
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


#' Convert spreadsheet range to a vector of row-col strings.
#'
#' This is an internal function that converts a spreadsheet range to a vector of row-col strings.
#' @param x a string representing a spreadsheet range

string_to_range <- function(string){
  
  limits_df <- 
    cellranger::as.cell_limits(string) %>% 
    as.data.frame() %>% 
    tidyr::expand_grid(row = c(.$ul[[1]]:.$lr[[1]]), 
                       col = c(.$ul[[2]]:.$lr[[2]]))
  
  range <- paste0(limits_df$row,"-",limits_df$col)
  
  range
}

#' Convert  range to a filtering quosure 
#' 
#' Convert an expression that represents a spreadsheet range to a quosure 
#' that can be used to create a variable that returns TRUE if row-col 
#' combination is in the range.
#' 
#' @param x a string expression

string_expressions_to_quosures <- function(string_expression,environ){
  
  symbol_expression %>% map(function(x){
    
  text <- paste0('paste0(row,"-",col) %in% string_to_range("',x[[2]],'")')
  
  string_quo <- rlang::as_quosure(rlang::parse_expr(text),env = environ)
  
  string_quo }
  )
}

#' Convert symbol to a filtering quosure 
#' 
#' Convert an expression that represents a fmt_ function to a quosure 
#' that can be used to create a variable that returns TRUE if row-col 
#' combination is in the range.
#' 
#' @param symbol_expression a symbol expression

symbol_expressions_to_quosures <- function(symbol_expression, environ){
  
  symbol_expression %>% map(function(x){
    function_text <- 
      paste0('purrr::invoke(unpivotr::',
             rlang::as_label(x),
             ', format_id_vec = local_format_id,sheet_format = format)')
    
    filter_quosures_symbol <- 
      rlang::as_quosure(rlang::parse_expr(function_text),env = environ)
    
    filter_quosures_symbol
  })
  

  
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


#' Get name from a language object 
#'
#' This is an internal function that adds a prefixed name to a quosure so that variables added to a data frame using this quosure have a predictable name.
#' @param x a language

name_language_expressions <- function(x, prefix) {
    
  purrr::map_chr(x,function(x){
    paste0(
      prefix,
      x %>% rlang::as_label() %>% make.names() %>%
        stringr::str_replace_all("\\.+", ".") %>% stringr::str_remove_all("(\\.$)|(^X\\.)") %>%
        stringr::str_replace_all("\\.", "_") %>%
        ifelse(stringr::str_sub(., start = 1, 1) %in% as.character(0:9), paste0("x", .), .)
     )
   }
 )
}


#' Get name from a vector of spreadsheet range strings 
#'
#' This is an internal function that adds a prefixed name to a quosure so that variables added to a data frame using this quosure have a predictable name.
#' @param x a vector of strings representing spreadsheet ranges 


name_string_expressions <- function(x,prefix){
  x %>% purrr::map(rlang::get_expr)  %>% unlist() %>% 
    stringr::str_remove("\\:") %>% paste0(prefix,.) 
} 


#' Get name from a vector of symbols 
#'
#' This is an internal function that adds a prefixed name to a quosure so that variables added to a data frame using this quosure have a predictable name.
#' @param x a vector of symbols

name_symbol_expressions <- function(symbols,prefix){
    purrr::map(symbols,rlang::as_label)  %>% unlist() %>%stringr::str_remove("\\:") %>% paste0(prefix,.) 
}
