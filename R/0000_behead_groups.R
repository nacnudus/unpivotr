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
    
    if(is.null(attr(sheet,"data_cells"))){
      
      sheet <- 
        sheet %>% locate_data(sheet = .,!is.na(numeric))
      
      corner_refs <-  unpivotr::get_corner_cell_refs(attr(sheet,"data_cells"))
      
      rows_index <- 
        data_frame(LETTERS) %>% mutate(letters2 = map(1:length(.),function(x) LETTERS)) %>% 
        unnest()  %>% 
        mutate(final = paste0(LETTERS,letters2)) %>% pull(final) %>% 
        c(LETTERS,.)
      
      min_letter <- rows_index[corner_refs$min_col]
      max_letter <- rows_index[corner_refs$max_col]
      
      warning(paste0("Data cells have yet not been identified in this this dataframe with `locate_data()`.",
                     " Continuing using defaults of `locate_data()`. Data cells have been located at :",
                     min_letter,corner_refs$min_row,":",max_letter,corner_refs$max_row))
      
    }
    
    
    formats <- attr(sheet, "formats")
    tabledata <- attr(sheet, "data_cells")
    
    added_var_list <- list(sheet,".header_label",".direction", ".value")
    
    sheet <-  added_var_list %>% reduce(add_variable_if_missing)
    
    # Get col groups ----
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <-   match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_col_header_direction_temp <- default_col_header_direction
    
    min_header_index_temp <-     
      sheet$.header_label %>% str_remove_all("[a-z]+|[:punct:]+") %>% 
      as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1)
    
    
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp,header_fill = header_fill_choice,
                                 default_col_header_direction = default_col_header_direction_temp,
                                 min_header_index = min_header_index_temp)    
    # Get rows groups ----
    
    value_ref <-   get_corner_cell_refs(tabledata)
    header_fill_choice <- match.arg(arg = header_fill, choices = c("local_format_id","style","borders","none"))  
    groupings_temp <- .groupings
    default_row_header_direction_temp <- default_row_header_direction
    
    
    min_header_index_temp <-     
      col_groups$.header_label %>% str_remove_all("[a-z]+|[:punct:]+") %>% 
      as.numeric() %>% max(.,na.rm = TRUE) %>% ifelse(is.finite(.),.,0) %>% `+`(1)
    
    
    
    row_groups <- get_row_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 header_fill = header_fill_choice,
                                 default_row_header_direction = default_row_header_direction_temp,
                                 table_data = tabledata,
                                 min_header_index = min_header_index_temp)
    
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
    
    orientated_df_nested <-
      sheet %>%
      filter(!is.na(.direction)) %>% 
      group_by(.direction, .header_label) %>%
      mutate(value = coalesce(character,as.character(numeric))) %>%
      select(row,col,.value,.direction,.header_label) %>%
      nest()
    
    data_row_index <- which(orientated_df_nested$.direction == "data")
    header_dfs   <- orientated_df_nested$data[orientated_df_nested$.direction != "data"]
    directions <- orientated_df_nested$.direction[orientated_df_nested$.direction != "data"]
    header_names <- orientated_df_nested$.header_label[orientated_df_nested$.direction != "data"]
    data_cells <- attr(sheet, "data_cells") %>% select(row,col,.value)
    
    header_dfs <- 
      map2(header_dfs,header_names, function(header_df, header_name){
        header_df %>% 
          rename(!!sym(header_name) := .value)
      })
    
    tidy_df <- 
      list(
        x = header_dfs,
        y = directions) %>%
      pmap(function(x,y){
        enhead_tabledata(header_data = x, direction = y,
                         values = data_cells)} ) %>%
      reduce(full_join,by = c("row", "col",".value"))  
    
    attr(sheet,"data_cells") <- 
    attr(sheet,"data_cells") %>% 
      left_join(tidy_df, by = c("row","col",".value"))
    
    sheet <- 
    sheet %>% filter(is.na(.direction)) 
    
    sheet 
    
    
    
    
    
    
    
    
    
    
  }
