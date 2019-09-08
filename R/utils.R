#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom methods is
#' @importFrom utils View installed.packages

# Spurious imports to satisfy R CMD check
#' @importFrom purrr map

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

# Concatenate lists into vectors, handling factors and NULLs, and coercing data
# types only when necessary
concatenate <- function(..., combine_factors = TRUE, fill_factor_na = TRUE) {
  c.POSIXct <- function(..., recursive = FALSE) {
    .POSIXct(c(unlist(lapply(list(...), unclass))), tz = "UTC")
  }
  dots <- (...)
  dots_is_null <- purrr::map_lgl(dots, rlang::is_null)
  # If all elements are NULL, return as-is
  if (all(dots_is_null)) {
    return(dots)
  }
  # If any non-NULL elements aren't scalars, return as-is
  dots_is_scalar_vector <- purrr::map_lgl(dots, rlang::is_scalar_vector)
  if (any(!dots_is_scalar_vector[!dots_is_null])) {
    return(dots)
  }
  classes <- purrr::map(dots, class)
  # It might be safe to use c() if all non-NA/NULLs are the same class.
  if (length(unique(classes[!dots_is_null])) == 1L) {
    # The first element of each class is the telling one
    all_classes <- classes[!dots_is_null][[1]]
    first_class <- all_classes[1]
    # If it's a factor, then forcats::fct_c() could combine the levels if so
    # desired.
    if (first_class %in% c("factor", "ordered")) {
      # If combining_factors then forcats::fct_c() needs all elements to be
      # factors, so replace them each with an NA factor. Or even if you're not
      # combining factors but still want some kind of consistency.
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
      # c() omits NULLs, so replace them with NA, which c() will promote when
      # necessary.
      dots[dots_is_null] <- NA
      dots <- do.call(c, dots)
      # c() demotes dates etc. when the first element is NA, so replace the
      # classes.
      class(dots) <- all_classes
      return(dots)
    }
  }
  # Here, not every non-NA/NULL element is the same class, and c() isn't very
  # clever about homogenising things, so handle factors and dates manually.
  # c() ignores nulls, so replace them with NA.
  dots[dots_is_null] <- NA
  # Convert factors to strings before they're (potentially) coerced to integers
  factors <- purrr::map_lgl(classes, ~ .[1] %in% c("factor", "ordered"))
  dots[factors] <- purrr::map(dots[factors], as.character)
  # Convert dates to strings before they're (potentially) coerced to numbers
  dates <- purrr::map_lgl(classes, ~ .[1] %in% c("Date", "POSIXct", "POSIXlt"))
  dots[dates] <- purrr::map(dots[dates], format, justify = "none", trim = TRUE)
  # Finally go with c()'s default homegnising of remaining classes.  Don't use
  # purrr::flatten(), because it strips classes from dates.
  do.call(c, dots)
}

# Return an NA of the same type as the given vector
na_types <- list(
  logical = NA,
  integer = NA_integer_,
  double = NA_real_,
  character = NA_character_,
  complex = NA_complex_
)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))

# Apply custom functions to list-elements of a list-column created by pack()
# whose type matches the custom function.
maybe_format_list_element <- function(x, name, functions) {
  func <- functions[[name]]
  if (is.null(func)) func <- identity
  func(x)
}

# Standardise dialects of directions
standardise_direction <- function(direction) {
  stopifnot(length(direction) == 1L)
  dictionary <-
    c(`up-left` = "up-left", `up` = "up", `up-right` = "up-right",
      `right-up` = "right-up", `right` = "right", `right-down` = "right-down",
      `down-right` = "down-right", `down` = "down", `down-left` = "down-left",
      `left-down` = "left-down", `left` = "left", `left-up` = "left-up",
      `up-ish` = "up-ish", `right-ish` = "right-ish",
      `down-ish` = "down-ish", `left-ish` = "left-ish",
      NNW = "up-left", N = "up", NNE = "up-right",
      ENE = "right-up", E = "right", ESE = "right-down",
      SSE = "down-right", S = "down", SSW = "down-left",
      WSW = "left-down", W = "left", WNW = "left-up",
      ABOVE = "up-ish", RIGHT = "right-ish",
      BELOW = "down-ish", LEFT = "left-ish")
  if (direction %in% names(dictionary)) return(unname(dictionary[direction]))
  stop("The direction \"", direction, "\" is not recognised.  See ?directions.")
}
