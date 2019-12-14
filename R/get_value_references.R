#' Get table references
#'
#' Identifies rectangle of numeric cells in table.
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`
#' @param manual_value_references sheet object read in by `tidyxl::xlsx_cells`
#'


get_value_references <- function(sheet, manual_value_references) {

  # Automatic producedure
  if (is.null(manual_value_references)) {
    ref_df <-
      sheet %>%
      dplyr::filter(!is.na(numeric)) %>%
      dplyr::summarise(
        min_row = min(row), max_row = max(row),
        min_col = min(col), max_col = max(col)
      )
    return(ref_df)
  }

  if (is.character(manual_value_references)) {
    # Use manual values

    cell_ref_df <- tibble::as_tibble(cellranger::as.cell_limits(manual_value_references))

    ref_df <-
      cell_ref_df[, 1:2] %>%
      purrr::set_names(c("min", "max")) %>%
      dplyr::mutate(dimension = c("row", "col")) %>%
      tidyr::gather(key, value, -dimension) %>%
      tidyr::unite(label, key, dimension, sep = "_") %>%
      tidyr::spread(label, value)

    return(ref_df)
  }

  

  if (rlang::is_formula(manual_value_references)) {
    current_quosure <- rlang::as_quosure(manual_value_references)

    sheet <-
      sheet %>%
      dplyr::filter(!!current_quosure)

    ref_df <-
      sheet %>%
      dplyr::summarise(
        min_row = min(row), max_row = max(row),
        min_col = min(col), max_col = max(col)
      )

    return(ref_df)
  }
}
