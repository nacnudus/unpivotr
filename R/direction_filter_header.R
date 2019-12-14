direction_filter_header  <- function(sheet,direction,value_ref){
  
  if(direction %in% c("up","N")){
    
    header_df <- 
      sheet %>%
      dplyr::filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
    
  }else if(direction %in% c("left","W")){
    
    col_df <- sheet %>% dplyr::filter(col <= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
    
    col_df_crns <- col_df %>%
      dplyr::filter(!is_blank) %>%
      get_corner_cell_refs()
    
    header_df <- sheet %>% dplyr::filter(row <= value_ref$max_row, row > col_df_crns$max_row, col < value_ref$min_col)

  }else if(direction %in% c("right","E")){
    
    col_df <- sheet %>% dplyr::filter(col >= value_ref$max_col, col >= value_ref$min_col, row < value_ref$min_row)
    
    col_df_crns <- col_df %>%
      dplyr::filter(!is_blank) %>%
      get_corner_cell_refs()
    
    header_df <- sheet %>% dplyr::filter(row <= value_ref$max_row, row > col_df_crns$max_row, col > value_ref$max_col)
    
  }else if(direction %in% c("down","S")){
    
    header_df <- 
      sheet %>%
      dplyr::filter(col <= value_ref$max_col, col >= value_ref$min_col, row > value_ref$max_row)
    
    
  }else {
    
    stop("Please check the direction you are providing")
    
  }
}