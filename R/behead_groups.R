#' Behead multiple header groups 
#' @description
#' Beheads multiple headers defined according to expressions in .groupings. 
#' @param sheet data frame created by xlsx_cells
#' @param type  indicating which type of headers are to be labelled. Options include "row", "col", "both" and "meta". 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#'
#' @export


behead_groups <-
  function(sheet= NULL, type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    
    sheet_temp = sheet
    type_temp = type
    .groupings_temp = .groupings
    default_col_header_direction_temp = default_col_header_direction
    default_row_header_direction_temp = default_row_header_direction
    header_fill_temp = header_fill
    
    
    behead_groups_if(sheet= sheet_temp ,type = type_temp, .groupings = .groupings_temp,
                     default_col_header_direction = default_col_header_direction_temp,
                     default_row_header_direction = default_row_header_direction_temp,
                     header_fill = header_fill_temp)
    
  }
