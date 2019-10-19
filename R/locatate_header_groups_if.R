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
  function(sheet= NULL, direction = "N", .groupings = groupings(fmt_alignment_indent), .hook_if = hook(any(FALSE)),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {

    
      sheet_temp  <-  sheet
      direction_temp <- direction 
      .groupings_temp <-  .groupings
      .hook_if_temp <- .hook_if
      default_col_header_direction_temp <- default_col_header_direction
      default_row_header_direction_temp <- default_row_header_direction
      header_fill_temp <- header_fill
      
      
      
      locate_groups_if(sheet= sheet_temp, direction = direction_temp, .groupings = .groupings_temp,.hook_if = .hook_if_temp,
                              default_col_header_direction = default_col_header_direction_temp,
                              default_row_header_direction = default_row_header_direction_temp,
                              header_fill = header_fill_temp)
      
    
    
    
  }


#' Conditionally locate multiple header groups 
#' @description
#' Adds annotations to the data frame, indicating which cells are headers and how they relate to data cells
#' @param sheet data frame created by xlsx_cells
#' @param type  indicating which type of headers are to be labelled. Options include "row", "col", "both" and "meta". 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param ... filter expression that identifies headers.
#' @export

locate_groups_if <-
  function(sheet= NULL,..., direction = "N", .groupings = groupings(fmt_alignment_indent), .hook_if = hook(any(FALSE)),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    
    # If data_cells are missing, locate them automatically  
    if(is.null(attr(sheet,"data_cells"))){
      # Set data location 
      sheet <- 
      sheet %>% locate_data(sheet = .,!is.na(numeric))
      
      # Report location of data cells 
      corner_refs <-  get_corner_cell_refs(attr(sheet,"data_cells"))
      min_letter <- unpivotr::cols_index[corner_refs$min_col]
      max_letter <- unpivotr::cols_index[corner_refs$max_col]
      
      warning(paste0("Data cells have yet not been located in this this dataframe with `locate_data()`.",
             " Continuing using defaults of `locate_data()`. Data cells have been located at :",
              min_letter,corner_refs$min_row,":",max_letter,corner_refs$max_row))
  
    }
    # Add filtering if statements 
    if(length(rlang::quos(...))>0){
      filter_expr <- rlang::quos(...)
      
      filtered_header_cells <- sheet %>% dplyr::filter(!!!filter_expr)
    } 


    # Assign attributes to objects  
    formats <- attr(sheet, "formats")
    tabledata <- attr(sheet, "data_cells")
   
    # Add annotation variables if missing  
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    sheet <-  added_var_list %>% purrr::reduce(add_variable_if_missing)
   
    # Get col groups ---------------------------------------------------------------------------------------------------
    # Get arguments for get_col_groups 

    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <-   match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_col_header_direction_temp <- default_col_header_direction
    .hook_if_temp <- .hook_if  
    
    
    min_header_index_temp <- suppressWarnings(get_header_index(sheet$.header_label,"^col_header"))  
    
   header_groups <- get_header_groups(sheet, direction, value_ref, formats,
                                  .groupings = groupings_temp,
                                  .hook_if = .hook_if_temp,
                                  header_fill = header_fill_choice,
                                  default_col_header_direction = default_col_header_direction_temp,
                                  table_data = tabledata,
                                  min_header_index = min_header_index_temp) 
      
    min_header_index_temp <- suppressWarnings(get_header_index(sheet$.header_label,"^col_header"))  
    
    
    # if(exists("filtered_header_cells") & (type == "both" | type == "col")){
    #   col_groups_in_filter <- 
    #     paste0(col_groups$row,unpivotr::cols_index[col_groups$col]) %in% 
    #     paste0(filtered_header_cells$row,unpivotr::cols_index[filtered_header_cells$col])
    #   
    #   col_groups <- col_groups %>% dplyr::filter(col_groups_in_filter)
    #   
    # }
    
    
    # Create column values 
      
      sheet <- sheet %>% dplyr::left_join(header_groups, by = c("row","col"),suffix = c(".o",".n")) 
      
      sheet <- 
        sheet %>% 
        dplyr::mutate(.header_label = dplyr::coalesce(.header_label.n,.header_label.o)) %>% 
        dplyr::mutate(.direction = dplyr::coalesce(.direction.n,.direction.o)) %>% 
        dplyr::mutate(.value = dplyr::coalesce(.value.n,.value.o)) %>% 
        dplyr::select(-.value.n,-.value.o,-.direction.n,-.direction.o,-.header_label.n,-.header_label.o)
        
    attr(sheet, "formats") <- formats
    attr(sheet, "data_cells") <- tabledata
    
    sheet 

  }




