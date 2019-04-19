magrittr::`%>%`




NULL

globalVariables(c(
  ".",
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
  "is_na",
  ".value",
  ".data_type",
  "n",
  ":=",
  ".partition",
  "ns_env",
  "corner_row",
  "corner_col",
  ".boundary"
))

concatenate <- function(..., combine_factors = TRUE, fill_factor_na = TRUE) {
  c.POSIXct <- function(..., recursive = FALSE) {
    .POSIXct(c(unlist(lapply(list(...), unclass))), tz = "UTC")
  }
  dots <- (...)
  dots_is_null <- purrr::map_lgl(dots, rlang::is_null)
  if (all(dots_is_null)) {
    return(dots)
  }
  dots_is_scalar_vector <- purrr::map_lgl(dots, rlang::is_scalar_vector)
  if (any(!dots_is_scalar_vector[!dots_is_null])) {
    return(dots)
  }
  classes <- purrr::map(dots, class)
  if (length(unique(classes[!dots_is_null])) == 1L) {
    all_classes <- classes[!dots_is_null][[1]]
    first_class <- all_classes[1]
    if (first_class %in% c("factor", "ordered")) {
      if (combine_factors || fill_factor_na) {
        dots[dots_is_null] <- list(factor(NA_character_))
      }
      if (combine_factors) {
        return(forcats::fct_c(rlang::splice(dots)))
      }
      else {
        return(dots)
      }
    } else {
      dots[dots_is_null] <- NA
      dots <- do.call(c, dots)
      class(dots) <- all_classes
      return(dots)
    }
  }
  dots[dots_is_null] <- NA
  factors <- purrr::map_lgl(classes, ~ .[1] %in% c("factor", "ordered"))
  dots[factors] <- purrr::map(dots[factors], as.character)
  dates <- purrr::map_lgl(classes, ~ .[1] %in% c("Date", "POSIXct", "POSIXlt"))
  dots[dates] <- purrr::map(dots[dates], format, justify = "none", trim = TRUE)
  do.call(c, dots)
}

na_types <- list(
  logical = NA,
  integer = NA_integer_,
  double = NA_real_,
  character = NA_character_,
  complex = NA_complex_
)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))

maybe_format_list_element <- function(x, name, functions) {
  func <- functions[[name]]
  if (is.null(func)) func <- identity
  func(x)
}
