#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import data.table
#' @import tidyr

#' @importFrom methods is
#' @importFrom utils View installed.packages

# Spurious imports to satisfy R CMD check
#' @importFrom dtplyr tbl_dt
#' @importFrom purrr map

NULL

globalVariables(c(".",
                  "inner_join",
                  "mutate",
                  "select",
                  "rename",
                  "quo",
                  "UQ",
                  "quo_name",
                  "from_row",
                  "from_col",
                  "to_row",
                  "to_col",
                  "type",
                  "value",
                  "everything",
                  "data_type",
                  ".boundary"))

# Wrapper of c() and fct_c() to dispatch the correct concatenation function
concatenate <- function(x) {
  if(length(x) == 0) return(x)
  if(!is.list(x)) return(c(x))
  # Replace any NAs with NULLs, then all NULLs with a value from above or below
  # (for the sake of concatentation), then put the NULLs/NAs back
  missings <- union(which(purrr::map_lgl(x, is.null)),
                    which(purrr::map_lgl(x, anyNA, recursive = FALSE)))
  x[missings] <- list(NULL)
  x <-
    tibble::tibble(x) %>%
    tidyr::fill(x, .direction = "down") %>%
    tidyr::fill(x, .direction = "up") %>%
    dplyr::pull(1)
  # Dispatch the appropriate concatenation function
  if(is.factor(x[[1]])) {
    x <- fct_c(x)
  } else {
    x <- do.call(c, x)
  }
  # Put back the NULLs/NAs
  if(is.list(x)) {
    x[missings] <- list(NULL)
  } else {
    x[missings] <- NA
  }
  return(x)
}

# Return an NA of the same type as the given vector
na_types <- list(logical = NA,
                 integer = NA_integer_,
                 double = NA_real_,
                 character = NA_character_,
                 complex = NA_complex_)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))
