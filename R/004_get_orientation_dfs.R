#' Produces a data frame with a row for each col/row combination in a the provided range
#' and with direction information extracted from the name provided by the orientation object.
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range
#' and with direction information extracted from the name provided by the orientation object.#'
#' @param orientation  a vector of length one representing a direction (N,NNW,W, WNW, etc.) that has a corresponding name that represents a range (e.g A2:B11).
#' For example, [c("C2:D2" = "N")]
#'
#' @name get_orientation_df
#' @examples
#'

get_orientation_df <- function(orientation){

  names(orientation) %>%
    get_range_dfs() %>%
    mutate(.orientation = orientation)
  
}


#' Produces a data frame with a row for each col/row combination in a the provided range
#' and with direction information extracted from the name provided by the orientation object.
#' @description
#' This function produces a data frame with a row for each col/row combination in a the provided range
#' and with direction information extracted from the name provided by the orientation object.#'
#' @param orientation  a vector of abritrary length representing  directions (N,NNW,W, WNW, etc.) that have a corresponding names that represent ranges (e.g A2:B11).
#' For example, [c("C1:D1" = "N",  "C2:D2" = "N", "B3:B4" = "W" ,  "A3:A4" = "W",  "c3:d4" = "data")]
#'
#' @name get_orientation_df
#' @examples
#'
#'
get_orientations_df <- function(orientations){
  
  o_length <- length(orientations)
  
  tibble(.header_group_num = c(1:o_length)) %>%
    mutate(data = .header_group_num %>%
             map({ ~ orientations[.x] %>% get_orientation_df})) %>%
    mutate(.orientation = data %>% map_chr(~.x[[1,".orientation"]] ) ) %>%
    mutate(.header_group = ifelse(str_detect(.orientation,"^N"), "col_label","row_label")) %>%
    group_by(.header_group) %>%
    mutate(.header_group_name = paste(.header_group,str_pad(row_number(),width = 2,side = "left",pad = "0" ), sep = "_" )) %>%
    ungroup %>%
    select(data, .header_group = .header_group_name) %>%
    unnest() 
}
