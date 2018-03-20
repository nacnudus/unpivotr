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

# # Wrapper of c() and fct_c() to dispatch the correct concatenation function
# concatenate <- function(x) {
#   if(length(x) == 0) return(x)
#   if(!is.list(x)) return(c(x))
#   # Replace any NAs with NULLs, then all NULLs with a value from above or below
#   # (for the sake of concatentation), then put the NULLs/NAs back
#   is_factor <- is.factor(x[[1]])
#   is_list <- is.list(x)
#   na <- na_types[[typeof(do.call(c, x))]]
#   missings <- union(which(purrr::map_lgl(x, is.null)),
#                     which(purrr::map_lgl(x, anyNA, recursive = FALSE)))
#   x[missings] <- list(factor(NA))
#   # x <-
#   #   tibble::tibble(x) %>%
#   #   tidyr::fill(x, .direction = "down") %>%
#   #   tidyr::fill(x, .direction = "up") %>%
#   #   dplyr::pull(1)
#   # Dispatch the appropriate concatenation function
#   if(is_factor) {
#     x <- fct_c(x)
#   } else {
#     x <- do.call(c, x)
#   }
#   # Put back the NULLs/NAs
#   if(is_list) {
#     x[missings] <- list(NULL)
#   } else {
#     x[missings] <- na
#   }
#   return(x)
# }

# Concatenate lists into vectors, handling factors and NULLs, and coercing data
# types only when necessary
concatenate <- function(...) {
  dots <- (...)
  is_null_or_na <- purrr::map_lgl(dots, ~ is.null(.) || is.na(.))
  if(all(is_null_or_na)) return(rep(NA, length(dots)))
  classes <- purrr::map(dots, class)
  if(length(unique(classes[!is_null_or_na])) == 1L) {
    if(classes[!is_null_or_na][[1]][1] == "factor") {
      dots[is_null_or_na] <- list(factor(NA_character_))
      return(forcats::fct_c(dots))
    } else {
      dots[is_null_or_na] <- NA
      return(do.call(c, dots))
    }
  }
  dots[is_null] <- NA
  # Convert factors to strings before they're (potentially) coerced to integers
  factors <- purrr::map_lgl(classes, ~ .[1] == "factor")
  dots[factors] <- purrr::map(dots[factors], as.character)
  do.call(c, dots)
}

# Return an NA of the same type as the given vector
na_types <- list(logical = NA,
                 integer = NA_integer_,
                 double = NA_real_,
                 character = NA_character_,
                 complex = NA_complex_)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))
