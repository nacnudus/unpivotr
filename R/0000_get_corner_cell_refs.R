#' get_corner_cell_refs
#'
#' Identifies corners of sheet 
#'
#' @param sheet sheet object read in by `tidyxl::xlsx_cells`





get_corner_cell_refs <- function(sheet) {
  
  # Automatic producedure
    ref_df <- 
      sheet %>% 
      summarise(
        min_row = min(row), max_row = max(row),
        min_col = min(col), max_col = max(col)
      ) 
    return(ref_df)
    
  } 