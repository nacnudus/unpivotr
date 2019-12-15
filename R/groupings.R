#' Groupings 
#' @description 
#' This functions passes grouping expressions to the .groupings argument of locate_groups. It works most naturally with fmt_* functions.
#' @param ... mutate expression to group headers.
#' @export
#' 

groupings <- function(...){
  
  rlang::quos(...)
  
}
