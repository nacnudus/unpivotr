#' Behead multiple header groups 
#' @description
#' Beheads multiple headers defined according to expressions in .groupings. 
#' @param sheet a tidyxl data frame.
#' @param direction  a string indicating which type of headers are to be labeled. Options include compass direction or up/down/left/right. 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param .hook_if expression determining whether directions are hooked.
#' @param .hook_if_rev expression determining whether directions are reverse hooked.
#'
#' @export
#' @examples print("todo")
#' 
#' 
#' 
#' 
#' 
#' 

behead_groups <-
  function(sheet= NULL, direction = "W", .groupings = groupings(fmt_alignment_indent),
           .hook_if = hook(any(FALSE)),.hook_if_rev = hook(any(FALSE)),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    sheet_temp = sheet
    direction_temp = direction
    .hook_if_temp = .hook_if
    .hook_if_rev_temp = .hook_if_rev
    .groupings_temp = .groupings
    default_col_header_direction_temp = default_col_header_direction
    default_row_header_direction_temp = default_row_header_direction
    header_fill_temp = header_fill
    
    
    behead_groups_if(sheet= sheet_temp ,direction = direction_temp, .groupings = .groupings_temp,
                     .hook_if = .hook_if_temp, .hook_if_rev = .hook_if_rev_temp,
                     default_col_header_direction = default_col_header_direction_temp,
                     default_row_header_direction = default_row_header_direction_temp,
                     header_fill = header_fill_temp)
  }
