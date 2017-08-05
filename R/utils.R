#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import data.table
#' @import tidyr

#' @importFrom methods is

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
                  ".boundary"))
