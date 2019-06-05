#' Fill in blanks
#'
#' This function ensures that merged cells are unmerged.
#'
#' @param header_df a data frame containing header cells. 
#' @param col_header_fill the manner is which blank cells will be filled. 
#' @param formats the formats associated with the workbook containing the header_df cells.
#' 

 
  reduce_mutated <- function(df, form_list,format){

    current_quosure <-  form_list[[1]]
    var_name_sym <-  sym(form_list[[2]])
    
     df %>% 
      mutate(!!var_name_sym:= !!current_quosure)  
     
     
     
     
     
     
  }  
  
  
  
  