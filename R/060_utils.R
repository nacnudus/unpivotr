#' paste3
#'
#' Removes NAs from paste. Taken from https://stackoverflow.com/users/1855677/42 stackoverflow answer.
#'
#' @param table_components object returned by `process_ABS_sheet`
#'
#' @export

paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

