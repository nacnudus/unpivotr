#' Transform a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @description
#' This function is to be used following [unpivotr::migrate()].
#' It transforms a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @param orientated_df  a tidyxl data frame with a `.direction` and `.header.group` columns
#'
#' @name migrate
#' @export


locate <- function(cells, direction, name, values = NULL, types = data_type,
                      formatters = list(), drop_na = TRUE){
 
  added_var_list <- list(cells,".header_label",".direction", ".value")
  
  cells <-  added_var_list %>% reduce(add_variable_if_missing)
  
  cells %>% select(row,col,character) %>% spread(col,character)
  
  cells_f <- cells %>% filter(is.na(.direction)) %>% filter(!is.na(character) | !is.na(numeric)) 
  
  
  check_direction_behead(direction)
  check_distinct(cells)
  name <- rlang::ensym(name)
  functions <- purrr::map(formatters, purrr::as_mapper)
  values <- rlang::enexpr(values)
    values_was_null <- TRUE
  types <- rlang::ensym(types)

    
  type_names <- unique(dplyr::pull(cells_f, !!types))
  filter_expr <- direction_filter(direction)
  is_header <- rlang::eval_tidy(filter_expr, cells_f)
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
 
  headers_reshaped <-   
    headers %>% 
      tidyr::gather(.header_label, .value, -row,-col) %>% 
      mutate(.direction  = direction) 
  

  cells <-  cells %>% left_join(headers_reshaped, by = c("row","col"), suffix = c(".o",".n")) 
  
  cells <- 
    cells %>% 
    mutate(.header_label = coalesce(.header_label.n,.header_label.o)) %>% 
    mutate(.direction = coalesce(.direction.n,.direction.o)) %>% 
    mutate(.value = coalesce(.value.n,.value.o)) %>% 
    select(-.value.n,-.value.o,-.direction.n,-.direction.o,-.header_label.n,-.header_label.o) 
  
  
  cells 

  
  
  
}
