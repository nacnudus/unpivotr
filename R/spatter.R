#' Spatter a key-values set across multiple columns.
#'
#' @param data A data frame.
#' @export
spatter <- function(.data, key, values = data_type) {
  UseMethod("spatter")
}

#' @export
spatter.data.frame <- function(cells, key, types = data_type) {
  key <- rlang::ensym(key)
  types <- rlang::ensym(types)
  type_names <- unique(dplyr::pull(cells, !! types))
  factors <-
    cells %>%
    dplyr::distinct(!! key, !! types) %>%
    dplyr::filter(!! types %in% c("fct", "ord")) %>%
    dplyr::pull(!! key)
  cells %>%
    pack(types = !! types, name = .value, drop_types = FALSE) %>%
    tidyr::spread(!! key, .value) %>%
    dplyr::mutate_if(is.list, concatenate) %>%
    # 2nd pass because factors are doubly listed
    dplyr::mutate_at(factors, concatenate)
}
