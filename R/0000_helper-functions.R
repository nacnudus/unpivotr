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



append_name_to_quosure <- function(x){
  list(x,
       paste0("grp_",
              x %>% as_label() %>% make.names() %>% 
                str_replace_all("\\.+",".") %>% str_remove_all("(\\.$)|(^X\\.)") %>% 
                str_replace_all("\\.","_") %>% 
                ifelse(str_sub(.,start = 1,1) %in% as.character(0:9),paste0("x",.),.  )))
  
}

