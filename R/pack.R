#' Pack cell values from separate columns per data type into one list-column
#'
#' @export
pack <- function(.data, types = data_type, name = "value", drop_types = TRUE,
                 drop_type_cols = TRUE) {
  types <- rlang::ensym(types)
  name <- rlang::ensym(name)
  type_colnames <- unique(dplyr::pull(.data, !! types))
  out <-
    .data %>%
    dplyr::mutate(!! name := purrr::map2(seq_len(n()),
                                         !! types,
                                         ~ (!! .data)[.x, .y, drop = TRUE]))
  names(out[[rlang::expr_text(name)]]) <- dplyr::pull(.data, !! types)
  if(drop_types && rlang::expr_text(types) != rlang::expr_text(name)) {
    out <- dplyr::select(out, - !! types)
  }
  if(drop_type_cols) {
    type_colnames <- setdiff(type_colnames, rlang::expr_text(name))
    out <- dplyr::select(out, - !! type_colnames)
  }
  out
}

#' Unpack cell values from one list-column into separate columns per data type
#'
#' @export
unpack <- function(.data, values = value, name = "data_type",
                   drop_packed = TRUE) {
  values <- rlang::ensym(values)
  name <- rlang::ensym(name)
  types <- map_chr(dplyr::pull(.data, !! values), cell_type)
  type_names <- unique(types)
  missings <- map(type_names,
                  ~ ifelse(.x %in% c("list", "fct", "ord"), list(NULL), NA))
  assignments <- map2(type_names,
                      missings,
                      ~ expr(ifelse(types == !! .x, !! values, !! .y)))
  names(assignments) <- type_names
  out <-
    dplyr::mutate(.data, !! name := types, !!! assignments) %>%
    dplyr::mutate_at(type_names, concatenate)
  if(drop_packed) out <- dplyr::select(out, - !! values)
  out
}

# Check the type of a list element, descending to the next level to check for
# lists of factors.
cell_type <- function(x) {
  out <- pillar::type_sum(x)
  if(out == "list") {
    list_type <- pillar::type_sum(x[[1]])
    if(list_type %in% c("fct", "ord", "list")) {
      return(list_type)
    }
  }
  return(out)
}
