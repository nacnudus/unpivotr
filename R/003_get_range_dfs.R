#' Produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @param range  a string representing a range in standard excel format. For example, "A4:Z15"
#'
#' @name get_range_df

get_range_df <- function(range){

  cell_ref_df <- as_tibble(cellranger::as.cell_limits(range))

  range_df <-
    cell_ref_df[,1:2] %>%
    set_names(c("min","max")) %>%
    mutate(dimension = c("row","col")) %>%
    gather(key, value, -dimension) %>%
    unite(label, key, dimension, sep = "_") %>%
    spread(label, value )

  expand.grid(row = c(range_df$min_row[1]:range_df$max_row[1]),
              col = c(range_df$min_col[1]:range_df$max_col[1]))

}

#' Produces a data frame with a row for each col/row combination in a the provided range.
#'
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range.
#' It handles instances where the range is a union of multiple ranges separate by commas.
#'
#' @param range  a string representing a range in standard excel format. For example, "A4:B15, C11:G22"
#'
#' @name get_range_dfs

get_range_dfs <- function(range){
  
  range %>%
    str_split(",") %>%
    unlist %>%
    map(get_range_df) %>%
    bind_rows()
}

