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

# Concatenate lists into vectors, handling factors and NULLs, and coercing data
# types only when necessary
concatenate <- function(..., combine_factors = TRUE, fill_factor_na = TRUE) {
  dots <- (...)
  if(max(purrr::map_int(dots, length)) > 1) {
    return(dots)
  }
  is_null_or_na <- purrr::map_lgl(dots, ~ is.null(.) || is.na(.))
  if(all(is_null_or_na)) return(rep(NA, length(dots)))
  classes <- purrr::map(dots, class)
  if(length(unique(classes[!is_null_or_na])) == 1L) {
    all_classes <- classes[!is_null_or_na][[1]]
    first_class <- all_classes[1]
    if(first_class %in% c("factor", "ordered")) {
      if(combine_factors) {
        dots[is_null_or_na] <- list(factor(NA_character_))
        return(forcats::fct_c(dots))
      } else {
        if(fill_factor_na) dots[is_null_or_na] <- list(factor(NA_character_))
        return(dots)
      }
    } else if(first_class == "raw") {
      dots[is_null_or_na] <- as.raw(0)
      return(do.call(c, dots))
    } else {
      dots[is_null_or_na] <- NA
      dots <- do.call(c, dots)
      class(dots) <- all_classes
      return(dots)
    }
  }
  dots[is_null_or_na] <- NA
  # Convert factors to strings before they're (potentially) coerced to integers
  factors <- purrr::map_lgl(classes, ~ .[1] %in% c("factor", "ordered"))
  dots[factors] <- purrr::map(dots[factors], as.character)
  # Convert dates to strings before they're (potentially) coerced to numbers
  dates <- purrr::map_lgl(classes, ~ .[1] %in% c("Date", "POSIXct", "POSIXlt"))
  dots[dates] <- purrr::map(dots[dates], format)
  do.call(c, dots)
}

# Return an NA of the same type as the given vector
na_types <- list(logical = NA,
                 integer = NA_integer_,
                 double = NA_real_,
                 character = NA_character_,
                 complex = NA_complex_)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))

# Apply custom functions to list-elements of a list-column created by pack()
# whose type matches the custom function.
maybe_format_list_element <- function(x, name, functions) {
  func <- functions[[name]]
  if(is.null(func)) func <- identity
  func(x)
}
