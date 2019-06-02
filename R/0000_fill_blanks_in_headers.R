
#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.

fill_blanks_in_headers <- function(header_df,col_header_fill, formats){
  
  if(col_header_fill ==  "style"){
     continue <- TRUE

    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = FALSE)

      continue <- !identical(sheet_original, header_df)
    }
    
  }
  
  
  if(col_header_fill ==  "local_format_id"){
    continue <- TRUE

    while (continue) {
      sheet_original <- header_df
      header_df <- header_df %>% unmerge_cells(strict_merging = TRUE)

      continue <- !identical(sheet_original, header_df)
    }
    
  }
  

  if(col_header_fill ==  "borders"){
  
    filled_join <- 
      header_df %>%  
        add_h_border_groups(formats) %>% 
        group_by(h_border_group)  %>%  
        select(row,col,h_border_group, character) %>% 
        mutate(value = ifelse(is.na(character),paste3(character, collapse = " _ ") %>% 
                                str_remove_all(" _ "),character)) %>% 
        ungroup() %>% 
        arrange(h_border_group, row,col ) %>% 
        select(row,col, character = value) 
  
    header_df <- 
      header_df %>% select(-character) %>% 
      left_join(filled_join, by = c("row", "col")) 

  }
  
 header_df  
  
} 
