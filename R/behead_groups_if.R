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

behead_groups_if <-
  function(sheet= NULL,..., type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    sheet_temp = sheet
    dotted_temp =  enquos(...) 
    type_temp = type 
    .groupings_temp = .groupings
    default_col_header_direction_temp = default_col_header_direction
    default_row_header_direction_temp = default_row_header_direction
    header_fill_temp = header_fill
    
    located_df <- 
    locate_header_groups_if(sheet= sheet_temp,!!!dotted_temp, type =type_temp, .groupings = .groupings_temp,
               default_col_header_direction = default_col_header_direction_temp,
               default_row_header_direction = default_row_header_direction_temp,
               header_fill = header_fill_temp) 
        
    
    data_cells <- located_df %>% attr("data_cells")
    header_cells <- located_df %>% filter(!is.na(.direction))
  
    attr(header_cells,"data_cells") <- data_cells
    data_cells <- header_cells %>% migrate()
    
    cells <- located_df %>% filter(is.na(.direction))
    attr(cells,"data_cells") <- data_cells
    attr(cells,"formats") <- attr(sheet,"formats")
    
    cells
    
  }
