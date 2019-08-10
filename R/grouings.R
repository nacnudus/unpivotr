#' Groupings 
#' @description 
#' This functions passes grouping expressions to the .groupings argument of locate_groups. It works most naturually with fmt_* functions.
#' @dots gouring expressions 
#'
#' @export

groupings <- function(...){
  
  enquos(...)
  
}
