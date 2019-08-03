#' get tidyABS components
#'
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
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
