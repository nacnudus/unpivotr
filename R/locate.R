#' Add direction annations to tidyxl data frame
#'
#' @description
#' This adds direction annations to tidyxl data frame.
#'
#' @param cells  a tidyxl data frame 
#' @param direction  direction of new headers
#' @param name  of variable associated with header
#' @param values  values
#' @param formatters  formatters
#' @param types  types
#' @param drop_na  drop_na
#' @export
#' @examples print("todo")

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
#' This function conditionally adds direction annotations to tidyxl data frame.
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
#' @examples print("todo")

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
