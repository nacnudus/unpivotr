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
  function(sheet= NULL,..., type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    

      sheet_temp  <-  sheet
      type_temp  <-  type
      .groupings_temp <-  .groupings
      default_col_header_direction_temp <- default_col_header_direction
      default_row_header_direction_temp <- default_row_header_direction
      header_fill_temp <- header_fill
      
      
      
      locate_header_groups_if(sheet= sheet_temp,type = "both", .groupings = .groupings_temp,
                              default_col_header_direction = default_col_header_direction_temp,
                              default_row_header_direction = default_row_header_direction_temp,
                              header_fill = header_fill_temp)
      
    
    
    
  }



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

locate_header_groups_if <-
  function(sheet= NULL,..., type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
 
    
    # If data_cells are missing, locate them automatically  
    if(is.null(attr(sheet,"data_cells"))){
      # Set data location 
      sheet <- 
      sheet %>% locate_data(sheet = .,!is.na(numeric))
      
      # Report location of data cells 
      corner_refs <-  unpivotr::get_corner_cell_refs(attr(sheet,"data_cells"))
      min_letter <- unpivotr::cols_index[corner_refs$min_col]
      max_letter <- unpivotr::cols_index[corner_refs$max_col]
      
      warning(paste0("Data cells have yet not been located in this this dataframe with `locate_data()`.",
             " Continuing using defaults of `locate_data()`. Data cells have been located at :",
              min_letter,corner_refs$min_row,":",max_letter,corner_refs$max_row))
  
    }
    
    
    # Add filtering if statements 
    if(length(enquos(...))>0){
      filter_expr <- enquos(...)
      
      filtered_header_cells <- sheet %>% filter(!!!filter_expr)
    } 
    
    
    
    # Assign attributes to objects  
    formats <- attr(sheet, "formats")
    tabledata <- attr(sheet, "data_cells")
   
    # Add annotation variables if missing  
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    sheet <-  added_var_list %>% reduce(add_variable_if_missing)
   
    # Get col groups ---------------------------------------------------------------------------------------------------
    # Get arguments for get_col_groups 
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <-   match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_col_header_direction_temp <- default_col_header_direction

    min_header_index_temp <-     
      sheet$.header_label %>% .[str_detect(.,"^col_header")] %>%  str_remove_all("[a-z]+|[:punct:]+") %>% 
      as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1)
    
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp,header_fill = header_fill_choice,
                                 default_col_header_direction = default_col_header_direction_temp,
                                 min_header_index = min_header_index_temp)
    
    
    if(exists("filtered_header_cells")){
      col_groups_in_filter <- 
        paste0(col_groups$row,unpivotr::cols_index[col_groups$col]) %in% 
        paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
      
      col_groups <- col_groups %>% filter(col_groups_in_filter)
      
    }
    
    
    # Get rows groups ----
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <- match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_row_header_direction_temp <- default_row_header_direction
    
    
    min_header_index_temp <-     
      sheet$.header_label %>% .[str_detect(.,"^row_header")] %>%  str_remove_all("[a-z]+|[:punct:]+") %>% 
      as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1)
    
    row_groups <- get_row_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 header_fill = header_fill_choice,
                                 default_row_header_direction = default_row_header_direction_temp,
                                 table_data = tabledata,
                                 min_header_index = min_header_index_temp)

    if(exists("filtered_header_cells")){
      row_groups_in_filter <- 
        paste0(row_groups$row,unpivotr::cols_index[row_groups$col]) %in% 
        paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
      
        row_groups <- row_groups %>% filter(row_groups_in_filter)
    }
    
    
    
  
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
