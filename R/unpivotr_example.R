

#' Get path to tidyABS example
#' tidyABS comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#' This function has been copied from the readxl package.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
unpivotr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "unpivotr")) %>% .[stringr::str_detect(., "xlsx$")]
  } else {
    system.file("extdata", path, package = "unpivotr", mustWork = TRUE)
  }
}
