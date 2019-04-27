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
    sheet %>%
      filter(!is.na(numeric)) %>%
      summarise(
        min_row = min(row), max_row = max(row),
        min_col = min(col), max_col = max(col)
      )
  } else {
    # Use manual values
    
    cell_ref_df <- as_tibble(cellranger::as.cell_limits(manual_value_references))
    
    cell_ref_df[,1:2] %>%
      set_names(c("min","max")) %>%
      mutate(dimension = c("row","col")) %>%
      gather(key, value, -dimension) %>%
      unite(label, key, dimension, sep = "_") %>%
      spread(label, value )
    
  }
}
