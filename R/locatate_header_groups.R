#' Locate multiple header groups 
#' @description
#' This function adds direction annations to tidyxl data frame, specifying annotions for several headers at once. 
#' The resulting data frame will contain the following columns: `.value`, `.direction`, and `.header_label`.
#' These columns indicate the value, direction and name of the header columns that will be produced in the tidy data frame produced by `migrate`.
#' 
#' @param sheet data frame created by xlsx_cells
#' @param direction  a string indicating which type of headers are to be labelled. Options include compass direction or up/down/left/right. 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' These expressions must be provided within `groupings`.
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param .hook_if expression determining whether directions are hooked. That is, whether a header cell to the left of data cells should be 
#'  annotated as "W" or "WNW". This argument contains an expression within the function `hook`. 
#'  This expression must evaluate to a single value for each header group. 
#'  For example, see below that any(fmt_alignment_indent == 0) is used rather that fmt_alignment_indent == 0.
#' @param .hook_if_rev expression determining whether directions are hooked. That is, whether a header cell to the left of data cells should be 
#'  annotated as "W" or "WNW". This argument contains an expression within the function `hook`. 
#'  This expression must evaluate to a single value for each header group.
#'  For example, see below that any(fmt_alignment_indent == 0) is used rather that fmt_alignment_indent == 0.
#'  
#' @export
#' @examples
#' 
#' library(tidyverse)
#' 
#' # Read in tidyxl data frame
#' xl_df <-  
#' unpivotr_example("worked-examples.xlsx") %>%
#' xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
#' append_fmt(fmt_alignment_indent) 
#'
#' # Add location annotations
#' xl_df <- 
#'  xl_df %>% 
#'   locate_data(data_type == "numeric") %>%
#'   locate_groups(direction = "W",
#'                 .groupings = groupings(fmt_alignment_indent),
#'                 .hook_if =     hook(any(fmt_alignment_indent == 0))) %>%
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


locate_groups <-
  function(sheet= NULL, direction = "N", .groupings = groupings(fmt_alignment_indent), 
           .hook_if = hook(any(FALSE)),.hook_if_rev = hook(any(FALSE)),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
    
    sheet_temp        <-  sheet
    direction_temp    <- direction 
    .groupings_temp   <-  .groupings
    .hook_if_temp     <- .hook_if
    .hook_if_rev_temp <- .hook_if_rev
    default_col_header_direction_temp <- default_col_header_direction
    default_row_header_direction_temp <- default_row_header_direction
    header_fill_temp <- header_fill
    
    locate_groups_if(sheet= sheet_temp, direction = direction_temp, .groupings = .groupings_temp,
                     .hook_if = .hook_if_temp,
                     .hook_if_rev = .hook_if_rev_temp,
                     default_col_header_direction = default_col_header_direction_temp,
                     default_row_header_direction = default_row_header_direction_temp,
                     header_fill = header_fill_temp)
  }


#' Locate multiple header groups 
#' @description
#' This function conditionally adds direction annations to tidyxl data frame, specifying annotions for several headers at once. 
#' The resulting data frame will contain the following columns: `.value`, `.direction`, and `.header_label`.
#' These columns indicate the value, direction and name of the header columns that will be produced in the tidy data frame produced by `migrate`.

#' @param sheet data frame created by xlsx_cells
#' @param direction  a string indicating which type of headers are to be labelled. Options include compass direction or up/down/left/right. 
#' @param .groupings expressions representing how header cells are differentiated. Most naturally works with fmt_* functions. 
#' @param .hook_if expression determining whether directions are hooked. That is, whether a header cell to the left of data cells should be 
#'  annotated as "W" or "WNW". This argument contains an expression within the function `hook`. 
#'  This expression must evaluate to a single value for each header group. 
#'  For example, see below that any(fmt_alignment_indent == 0) is used rather that fmt_alignment_indent == 0.
#' @param .hook_if_rev expression determining whether directions are hooked. That is, whether a header cell to the left of data cells should be 
#'  annotated as "W" or "WNW". This argument contains an expression within the function `hook`. 
#'  This expression must evaluate to a single value for each header group.
#'  For example, see below that any(fmt_alignment_indent == 0) is used rather that fmt_alignment_indent == 0.
#' @param default_col_header_direction Indicates which direction is given to col headers by default. Only need if "NNW" is required, rather than "N". 
#' @param default_row_header_direction Indicates which direction is given to row headers by default. Only need if "WNW" is required, rather than "W". 
#' @param header_fill deals with merged cells. Fills in neighbouring cells if they have the same "local_format_id", "style" or are within "borders".
#' @param ... filter expression that identifies headers.
#' @export
#' @examples 
#' 
#' library(tidyverse)
#' 
#' # Read in tidyxl data frame
#' xl_df <- 
#'   unpivotr_example("anzsic.xlsx") %>%
#'    xlsx_cells_fmt(sheets = "Classes") %>%
#'    filter_fmt(row > 6) # Remove irrelevant rows  
#'    
#' # Identify data cells. 
#' xl_df <- 
#'   xl_df %>% 
#'    locate_data(col == 6 & is_blank == FALSE) 
#'    
#' # Add annotations for header cells that are numbers first, and then for header cells that are words.
#'  xl_df <- 
#'   xl_df %>%
#'    locate_groups_if(str_detect(character,"[0-9]") ,direction = "W", .hook_if = unpivotr::hook(TRUE)) %>% 
#'    locate_groups_if(str_detect(character,"[a-z]") ,direction = "W", .hook_if = unpivotr::hook(TRUE)) 

locate_groups_if <-
  function(sheet= NULL,..., direction = "N", .groupings = groupings(fmt_alignment_indent), 
           .hook_if = hook(any(FALSE)), .hook_if_rev = hook(any(FALSE)),
           default_col_header_direction = "N",default_row_header_direction = "W",header_fill = "local_format_id") {
   
    # If data_cells are missing, locate them automatically  
    if(is.null(attr(sheet,"data_cells"))){
      # Set data location 
      sheet <- 
        sheet %>% locate_data(sheet = .,!is.na(numeric))
      
      # Report location of data cells 
      corner_refs <-  get_corner_cell_refs(attr(sheet,"data_cells"))
      min_letter <- cellranger::letter_to_num(corner_refs$min_col)
      max_letter <- cellranger::letter_to_num(corner_refs$max_col)
      

      
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
    .hook_if_rev_temp <- .hook_if_rev  
    
    
    min_header_index_temp <- 
      sheet$.header_label %>% stringr::str_remove_all(paste0(direction,"_header_label_"))%>% 
      as.integer() %>% max(na.rm = T)  %>% ifelse(!is.finite(.),0,.)   
    
    header_groups <- get_header_groups(sheet, direction, value_ref, formats,
                                       .groupings = groupings_temp,
                                       .hook_if = .hook_if_temp,
                                       .hook_if_rev = .hook_if_rev_temp,
                                       header_fill = header_fill_choice,
                                       table_data = tabledata,
                                       min_header_index = min_header_index_temp) 
    
   
    if(exists("filtered_header_cells")){
       header_groups_row_cols <- paste0(header_groups$row, "-",header_groups$col)
       filtered_header_row_cols <- paste0(filtered_header_cells$row,"-", filtered_header_cells$col)
       
       header_groups <- header_groups %>% 
         dplyr::filter(header_groups_row_cols %in% filtered_header_row_cols)
      
    }
    
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

#' Supply an expression to identify which header groups are hooked.
#'
#' @description
#' This function is used with the `.hook_if` or `.hook_if_rev` arguments in the `locate_groups` function.
#' This expression must evaluate to a single value for each header group.
#' For example, see below that any(fmt_alignment_indent == 0) is used rather that fmt_alignment_indent == 0.
#'
#' It passes an expression to `dplyr::summarise` that identifies which header groups are hooked - for example swiched from N to NNW.
#' See the the `locate_groups` documentation for more information and an example.
#' 
#'
#' @param ...  expression applied to a  identifies which header groups are hooked
#'
#' @name get_range_dfs
#' @export
#' @examples
#' 
#' 
#' library(tidyverse)
#' 
#' # Read in tidyxl data frame
#' xl_df <-  
#' unpivotr_example("worked-examples.xlsx") %>%
#' xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
#' append_fmt(fmt_alignment_indent) 
#'
#' # Add location annotations
#' xl_df <- 
#'  xl_df %>% 
#'   locate_data(data_type == "numeric") %>%
#'   locate_groups(direction = "W",
#'                 .groupings = groupings(fmt_alignment_indent),
#'                 .hook_if =     hook(any(fmt_alignment_indent == 0))) %>%
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


hook <- function(...) {
  rlang::quos(...)
}


#' Groupings 
#' @description 
#' This functions passes grouping expressions to the .groupings argument of locate_groups. It works most naturally with fmt_* functions.
#' @param ... mutate expression to group headers.
#' @export
#' @examples
#' 
#' # Read in tidyxl data frame
#' xl_df <-  
#' unpivotr_example("worked-examples.xlsx") %>%
#' xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
#' append_fmt(fmt_alignment_indent) 
#'
#' # Add location annotations
#' xl_df <- 
#'  xl_df %>% 
#'   locate_data(data_type == "numeric") %>%
#'   locate_groups(direction = "W",
#'                 .groupings = groupings(fmt_alignment_indent),
#'                 .hook_if =     hook(any(fmt_alignment_indent == 0))) %>%
#'   locate(direction = "N", name = student) 
#'   
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each header variable has its own column.  
#'  xl_df %>% migrate()


groupings <- function(...){
  
  rlang::quos(...)
  
}



