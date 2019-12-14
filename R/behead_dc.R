#' Transform a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @description
#' This function is to be used following [unpivotr::migrate()].
#' It transforms a tidyxl data frame with directions to a tidy data frame that has a column for each header label.
#'
#' @param orientated_df  a tidyxl data frame with a `.direction` and `.header.group` columns
#'
#' @name migrate
#' @export


behead_dc <- function(cells, direction, name, values = NULL, types = data_type,
                      formatters = list(), drop_na = TRUE) {
  data_cells <- cells %>% attr("data_cells")
  formats <- cells %>% attr("formats")

  data_cells$dc <- 1


  direction_temp <- direction
  name_temp <- rlang::sym(name)
  values_temp <- values
  types_temp <- rlang::quo(types)
  formatters_temp <- formatters
  drop_na_temp <- drop_na

  temp_df <-
    dplyr::bind_rows(cells, data_cells) %>%
    behead(direction = direction_temp, name = !!name_temp)


  data_cells <- temp_df %>%
    dplyr::filter(dc == 1) %>%
    dplyr::select(-dc)

  cells <- temp_df %>%
    dplyr::filter(is.na(dc)) %>%
    dplyr::select(-dc) %>%
    dplyr::select(-!!name_temp)


  attr(cells, "data_cells") <- data_cells
  attr(cells, "formats") <- formats

  cells
}
