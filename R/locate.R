#' Add direction annations to tidyxl data frame
#'
#' @description
#' This function adds direction annations to tidyxl data frame. The resulting data frame will contain the following columns: `.value`, `.direction`, and `.header_label`.
#' These columns indicate the value, direction and name of the header columns that will be produced in the tidy data frame produced by `migrate`.
#' 
#' The header cells for which annotations are the outermost. 
#' For example, if "left" of "W" is used for the direction argument, cells furthest to the left in your spreadsheet will have location information added.
#'   
#'
#' @param cells  a tidyxl data frame 
#' @param direction  direction of new headers
#' @param name  of variable associated with header
#' @param values  values
#' @param formatters  formatters
#' @param types  types
#' @param drop_na  drop_na
#' @export
#' @examples 
#' # Read in a formatted tidyxl data frame. 
#' xl_df <- 
#' unpivotr_example("worked-examples.xlsx") %>%
#'  xlsx_cells_fmt(sheets = "pivot-annotations") 
#'  
#' # Identfy which cells are data cells using `locate_data` 
#'  
#'  xl_df <- xl_df %>% locate_data(data_type == "numeric") 
#'  
#'  # Identify header cells, indicating their direction with respect to the data and a name for their column in the final tidy data frame.
#'  xl_df <- 
#'   xl_df %>% 
#'    locate(direction = "WNW", name = subject_type) %>% 
#'    locate(direction = "W", name = subject) %>%
#'    locate(direction = "NNW", name = gender) %>% 
#'    locate(direction = "N", name = name)  
#'    
#'  # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()
#' 


locate <- function(cells, direction, name, values = NULL, types = data_type,
                   formatters = list(), drop_na = TRUE) {
  cells_temp <- cells
  direction_temp <- direction
  values_temp <- values
  formatters_temp <- formatters
  drop_na_temp <- drop_na
  
  
  locate_if(
    cells = cells_temp, direction = direction_temp, name = {{name}},
    values = values_temp, types = {{types}},
    formatters = formatters_temp, drop_na = drop_na_temp
  )
}


#' Conditionally adds direction annotations to tidyxl data frame
#'
#' @description
#' This function conditionally adds direction annations to tidyxl data frame. The resulting data frame will contain the following columns: `.value`, `.direction`, and `.header_label`.
#' These columns indicate the value, direction and name of the header columns that will be produced in the tidy data frame produced by `migrate`.
#' 
#' The header cells for which annotations are the outermost. 
#' For example, if "left" of "W" is used for the direction argument, cells furthest to the left in your spreadsheet will have location information added.
#'
#' The `...` arguments take explressions that identify which cells we have annotations added.
#'
#' @param cells  a tidyxl data frame 
#' @param direction  direction of new headers
#' @param name  of variable associated with header
#' @param values values
#' @param formatters formatters
#' @param drop_na drop_na
#' @param types types
#' @param ... expression to filter for headers 
#' @export
#' @examples
#'
#' # Read in a formatted tidyxl data frame. 
#' 
#' xl_df <- 
#'  unpivotr_example("worked-examples.xlsx") %>% 
#'  xlsx_cells_fmt(sheets = "pivot-hierarchy")
#'  
#' # Add a column indicate the leveling of indenting for each cell and locate data cell.
#' xl_df <- 
#'  xl_df %>% 
#'   append_fmt(fmt_alignment_indent) %>%
#'   locate_data(data_type == "numeric") %>%

#' # Add annotations for header cells. First for header cells to the left of the table with no indenting, and then for cells for one level of indenting.
#' xl_df <- 
#'  xl_df %>% 
#'   locate_if(fmt_alignment_indent == 0, direction = "WNW", name = subject_type) %>% 
#'   locate_if(fmt_alignment_indent == 1, direction = "W", name = subject) %>% 
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


locate_if <- function(cells, ..., direction, name, values = NULL, types = data_type,
                      formatters = list(), drop_na = TRUE) {

  
  # Store attributes (bind datacells and others if present)
  if (!is.null(attr(cells, "data_cells"))) {
    data_cells_attr <- attr(cells, "data_cells")
    data_cells_attr$dc <- 1
    cells <- dplyr::bind_rows(cells, data_cells_attr)
  }
  
  if (!is.null(attr(cells, "formats"))) {
    format <- attr(cells, "formats")
  }
  
  # Add Annotation variables
  added_var_list <- list(cells, ".header_label", ".direction", ".value")
  cells <- added_var_list %>% purrr::reduce(add_variable_if_missing)
  
  
  # Filter for cells without direction, but with character or numeric
  cells_f <- cells %>%
    dplyr::filter(is.na(.direction)) %>%
    dplyr::filter(!is.na(character) | !is.na(numeric))
  
  # Checks
  check_direction_behead(direction)
  check_distinct(cells)
  
  # Create variables
  name <- rlang::ensym(name)
  functions <- purrr::map(formatters, purrr::as_mapper)
  values <- rlang::enexpr(values)
  values_was_null <- TRUE
  types <- rlang::ensym(types)
  
  
  type_names <- unique(dplyr::pull(cells_f, !!types))
  
  # Use ... filter expressions if they have been provided
  # Otherwise, use direction filter
  if (length(rlang::enquos(...)) > 0) {
    filter_expr <- rlang::enquos(...)
    
    headers <- cells_f %>% dplyr::filter(!!!filter_expr)
    is_header_if <-
      paste0("R",cells_f$row,"C",cells_f$col) %in%
      paste0("R",headers$row,"C",headers$col)
    
    filter_expr <- direction_filter(direction)
    is_header_dir <- rlang::eval_tidy(filter_expr, cells_f)
    
    is_header <- is_header_dir & is_header_if
    
  } else {
    filter_expr <- direction_filter(direction)
    is_header <- rlang::eval_tidy(filter_expr, cells_f)
  }
  
  
  headers <-
    cells_f %>%
    dplyr::filter(is_header) %>%
    pack(types = !!types) %>%
    dplyr::mutate(
      is_na = is.na(value),
      !!name := purrr::imap(
        value,
        maybe_format_list_element,
        functions
      ),
      !!name := concatenate(!!name)
    ) %>%
    dplyr::filter(!(drop_na & is_na)) %>%
    dplyr::select(row, col, !!name)
  
  # no predicate filters, so discard all cells in the row/col of the headers
  data_cells <- dplyr::filter(cells_f, !is_header)
  
  
  # Prepare headers for join
  headers_reshaped <-
    headers %>%
    tidyr::gather(.header_label, .value, -row, -col) %>%
    dplyr::mutate(.direction = direction)
  
  # Join headers to original cells
  cells <- cells %>% dplyr::left_join(headers_reshaped, by = c("row", "col"), suffix = c(".o", ".n"))
  
  
  cells %>% dplyr::select(dplyr::matches("^\\."))
  
  # Unite updated/non-updated values
  cells <-
    cells %>%
    dplyr::mutate(.header_label = dplyr::coalesce(.header_label.n, .header_label.o)) %>%
    dplyr::mutate(.direction = dplyr::coalesce(.direction.n, .direction.o)) %>%
    dplyr::mutate(.value = dplyr::coalesce(.value.n, .value.o)) %>%
    dplyr::select(-.value.n, -.value.o, -.direction.n, -.direction.o, -.header_label.n, -.header_label.o)
  
  
  cells <- 
    cells %>% dplyr::select(.value, .direction, .header_label, dplyr::everything() )
  
  
  # Reattach data_cells and format if they exist
  if (exists("data_cells_attr")) {
    cells <- cells %>% dplyr::filter(is.na(dc))
    attr(cells, "data_cells") <- data_cells_attr
  }
  
  if (exists("format")) {
    attr(cells, "formats") <- format
  }
  
  cells
}
