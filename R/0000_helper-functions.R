#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#' 
reduce_mutated <- function(df, form_list){

  header_df %>% 
    mutate(!!sym(form_list[[2]]) := !!set_env(form_list[[1]])) 

} 

#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'

append_name_to_quosure <- function(x, prefix = "grp_"){
  list(x,
       paste0(prefix,
              x %>% as_label() %>% make.names() %>% 
                str_replace_all("\\.+",".") %>% str_remove_all("(\\.$)|(^X\\.)") %>% 
                str_replace_all("\\.","_") %>% 
                ifelse(str_sub(.,start = 1,1) %in% as.character(0:9),paste0("x",.),.  )))
  
}



#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'



add_variable_if_missing <- function(sheet, var){
  
  if(!var %in%  names(sheet)){
    var_sym <- sym(var)
    
    sheet <- sheet %>% mutate(!!var_sym := NA_character_)
  }
  
  sheet
}



#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
#'






string_range_to_filter_vars <- function(sheet,table_range){
  
  cell_ref_df <- as_tibble(cellranger::as.cell_limits(table_range))
  
  table_range_df <-
    cell_ref_df[,1:2] %>%
    set_names(c("min","max")) %>%
    mutate(dimension = c("row","col")) %>%
    gather(key, value, -dimension) %>%
    unite(label, key, dimension, sep = "_") %>%
    spread(label, value )
  
  string_filter_name <- sym(table_range %>% str_remove("\\:") %>% paste0("flt_",.))
  
  data_sheet <-
    sheet %>%
    mutate(!!string_filter_name := 
             row >= table_range_df$min_row[1] & 
             row <= table_range_df$max_row[1] & 
             col >= table_range_df$min_col[1] &
             col <= table_range_df$max_col[1])
  
}

