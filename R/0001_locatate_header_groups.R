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

locate_header_groups <-
  function(sheet= NULL, type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    formats <- attr(sheet, "formats")
    tabledata <- attr(sheet, "data_cells")
    
    added_var_list <- list(sheet,".header_label",".direction", ".value")
   
    sheet <-  added_var_list %>% reduce(add_variable_if_missing)
   
    # Get col groups ----
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <-   match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_col_header_direction_temp <- default_col_header_direction
    
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp,header_fill = header_fill_choice,
                                 default_col_header_direction = default_col_header_direction_temp)
    
    # Get rows groups ----
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <- match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_row_header_direction_temp <- default_row_header_direction
    
    row_groups <- get_row_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 header_fill = header_fill_choice,
                                 default_row_header_direction = default_row_header_direction_temp,
                                 table_data = tabledata)
    
    # Get metadata ----
    meta_df <- get_meta_df(sheet = sheet, value_ref = value_ref, formats = formats, col_groups = col_groups)

    # Create column values 
    if(type == "both" | type == "row"){
      
      sheet <- sheet %>% left_join(row_groups, by = c("row","col"),suffix = c(".o",".n")) 
      
      sheet <- 
        sheet %>% 
          mutate(.header_label = coalesce(.header_label.n,.header_label.o)) %>% 
          mutate(.direction = coalesce(.direction.n,.direction.o)) %>% 
          mutate(.value = coalesce(.value.n,.value.o)) %>% 
          select(-.value.n,-.value.o,-.direction.n,-.direction.o,-.header_label.n,-.header_label.o)
    }
    
    if(type == "both" | type == "col"){
      
      sheet <-  sheet %>% left_join(col_groups, by = c("row","col"), suffix = c(".o",".n")) 
      
      sheet <- 
        sheet %>% 
        mutate(.header_label = coalesce(.header_label.n,.header_label.o)) %>% 
        mutate(.direction = coalesce(.direction.n,.direction.o)) %>% 
        mutate(.value = coalesce(.value.n,.value.o)) %>% 
        select(-.value.n,-.value.o,-.direction.n,-.direction.o,-.header_label.n,-.header_label.o)
      
    }
    
    attr(sheet, "formats") <- formats
    attr(sheet, "data_cells") <- tabledata
    
    sheet 

  }
