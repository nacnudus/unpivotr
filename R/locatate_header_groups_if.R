#' Locate multiple header groups 
#' @description
#' Adds annotations to the data frame, indicating which cells are headers and how they relate to data cells
#' @param sheet data frame created by xlsx_cells
#' @param type  indicating which type of headers are to be labelled. Options include "row", "col", "both" and "meta". 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#'
#' @export

locate_groups <-
  function(sheet= NULL, type = "both", .groupings = groupings(fmt_alignment_indent),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {

      sheet_temp  <-  sheet
      type_temp  <-  type
      .groupings_temp <-  .groupings
      default_col_header_direction_temp <- default_col_header_direction
      default_row_header_direction_temp <- default_row_header_direction
      header_fill_temp <- header_fill
      
      
      
      locate_groups_if(sheet= sheet_temp,type = type_temp, .groupings = .groupings_temp,
                              default_col_header_direction = default_col_header_direction_temp,
                              default_row_header_direction = default_row_header_direction_temp,
                              header_fill = header_fill_temp)
      
    
    
    
  }


#' Conditionally locate multiple header groups 
#' @description
#' Adds annotations to the data frame, indicating which cells are headers and how they relate to data cells
#' @param sheet data frame created by xlsx_cells
#' @param dots  Expressions that filter header cells. 
#' @param type  indicating which type of headers are to be labelled. Options include "row", "col", "both" and "meta". 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#'
#' @export

locate_groups_if <-
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
    
    if(type == "both" | type == "col"){
      
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <-   match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_col_header_direction_temp <- default_col_header_direction

    min_header_index_temp <- suppressWarnings(get_header_index(sheet$.header_label,"^col_header"))  
    
    
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp,header_fill = header_fill_choice,
                                 default_col_header_direction = default_col_header_direction_temp,
                                 min_header_index = min_header_index_temp)
    
    }

    if(exists("filtered_header_cells") & (type == "both" | type == "col")){
      col_groups_in_filter <- 
        paste0(col_groups$row,unpivotr::cols_index[col_groups$col]) %in% 
        paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
      
      col_groups <- col_groups %>% filter(col_groups_in_filter)
      
    }
    
    
    # Get rows groups ----
    
    if(type == "both" | type == "row"){
      
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <- match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_row_header_direction_temp <- default_row_header_direction
    
    min_header_index_temp <- suppressWarnings(get_header_index(sheet$.header_label,"^row_header"))  
    
    row_groups <- get_row_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 header_fill = header_fill_choice,
                                 default_row_header_direction = default_row_header_direction_temp,
                                 table_data = tabledata,
                                 min_header_index = min_header_index_temp)
    
    }

    if(exists("filtered_header_cells") & (type == "both" | type == "row")){
      row_groups_in_filter <- 
        paste0(row_groups$row,unpivotr::cols_index[row_groups$col]) %in% 
        paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
      
        row_groups <- row_groups %>% filter(row_groups_in_filter)
    }
    
    
    # Get meta groups ----
    
    if(type == "meta"){
      
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <- match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_row_header_direction_temp <- default_row_header_direction
    
    min_header_index_temp <- suppressWarnings(get_header_index(sheet$.header_label,"^meta_header"))  
    
    meta_groups <- get_meta_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 header_fill = header_fill_choice,
                                 default_row_header_direction = default_row_header_direction_temp,
                                 table_data = tabledata,
                                 min_header_index = min_header_index_temp)
    
    }
    
    if(exists("filtered_header_cells") & type == "meta"){
      row_groups_in_filter <- 
        paste0(meta_groups$row,unpivotr::cols_index[meta_groups$col]) %in% 
        paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
      
      meta_groups <- meta_groups %>% filter(row_groups_in_filter)
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

    
    # Create column values 
    if(type == "meta"){
      
      sheet <- sheet %>% left_join(meta_groups, by = c("row","col"),suffix = c(".o",".n")) 
      
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




