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
#'
#'
#' @export

locate_header_groups <-
  function(sheet= NULL, 
           formats = NULL,
           table_range = NULL, manual_value_references = NULL, 
           added_row_groups = NULL,
           col_header_fill = "local_format_id",
           .groupings = groupings(fmt_alignment_indent),
           row_header_fill = "none",
           filter_col_headers_by = NULL,
           filter_row_headers_by = NULL,
           default_col_header_direction = "N",
           default_row_header_direction = "W",
           keep_meta_data = FALSE) {
    
    browser()
    
    tabledata <- attr(sheet, "data_cells")
    
    # Identify empty rows of data_cells
    
    empty_row_df <- 
      tabledata %>% 
      select(-comment) %>% 
      group_by(row)  %>% 
      summarise(empty_share = sum(.value == "")/n()) %>% 
      mutate(empty_share = ifelse(is.na(empty_share),1,empty_share))
    
    # tabledata <- 
    #   tabledata %>% filter(!row %in%  empty_row_df$row)
    
    # Get col groups ----
    
    value_ref <-   get_corner_cell_refs(tabledata)
    
    col_header_fill_choice <-   match.arg(arg = col_header_fill, choices = c("local_format_id","style","borders","none"))  
    
    groupings_temp <- .groupings
    
    default_col_header_direction_temp <- default_col_header_direction
    filter_col_headers_by_temp <- filter_col_headers_by 
    
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats, 
                                 .groupings = groupings_temp, 
                                 col_header_fill = col_header_fill_choice,
                                 default_col_header_direction = default_col_header_direction_temp,
                                 filter_col_headers_by = filter_col_headers_by_temp)
    
    
    
    # Get rows groups ----
    
    row_header_fill_choice <-   match.arg(arg = row_header_fill, choices = c("local_format_id","style","borders","none"))  
    group_row_headers_by_temp <- group_row_headers_by
    added_row_groups_temp <- added_row_groups
    
    default_row_header_direction_temp <-default_row_header_direction
    filter_row_headers_by_temp <- filter_row_headers_by
    
    
    row_groups <- get_row_groups(
      sheet = sheet, value_ref = value_ref, col_groups = col_groups, 
      formats = formats, added_row_groups = added_row_groups_temp, 
      group_row_headers_by = group_row_headers_by_temp, 
      row_header_fill = row_header_fill_choice,
      default_row_header_direction = default_row_header_direction_temp,
      table_data = tabledata,
      filter_row_headers_by = filter_row_headers_by_temp)
    
    
    
    
    # Get metadata ----
    meta_df <- get_meta_df(sheet = sheet, value_ref = value_ref, formats = formats, col_groups = col_groups)
    
    
    # Check that meta and col headers are unique ----
    # Get unique header groups ----
    unique_cols <- col_groups$data %>% map(3) %>% map(unique)
    unique_meta <- meta_df$data %>% map(3) %>% map(unique)
    unique_rows <- row_groups$data %>% map(3) %>% map(unique)
    
    
    # Remove meta data /col group duplicates ----
    if(length(unique_meta) > 0){
      
      joint_col <-
        full_join(
          tibble(values = unique_cols) %>%
            mutate(col_group = row_number()) %>%
            unnest(),
          tibble(values = unique_meta) %>%
            mutate(meta_group = row_number()) %>%
            unnest()
        )
      
      cols_to_keep <-
        joint_col %>%
        group_by(col_group) %>%
        filter(!is.na(col_group)) %>%
        summarise(keep = 1 != sum(!is.na(meta_group) / length(meta_group), na.rm = TRUE)) %>%
        pull(keep)
      
      
      col_groups <- col_groups[cols_to_keep, ]
      
    }
    
    
    # Prepare col groups for joining ----
    if(nrow(col_groups) > 0){
      col_groups <- col_groups %>% rename(.orientation = direction, .header_group = header_label) %>% select(.header_group,.orientation,data) %>% unnest()
      vars_to_combine <- names(col_groups)[!names(col_groups) %in% c(".header_group",".orientation","col","row")]
      col_groups <- col_groups %>% mutate(value = coalesce(!!!syms(vars_to_combine))) %>% select(-vars_to_combine)
    }
    # Prepare row groups for joining ----
    
    
    if(nrow(row_groups) > 0){
      row_groups <- row_groups %>% rename(.orientation = direction, .header_group = row_group) %>% select(.header_group,.orientation,data) %>% unnest()
      vars_to_combine <- names(row_groups)[!names(row_groups) %in% c(".header_group",".orientation","col","row")]
      row_groups <- row_groups %>% mutate(value = coalesce(!!!syms(vars_to_combine))) %>% select(-vars_to_combine)
    }
    
    # Prepare meta groups for joining ----
    
    if(nrow(meta_df) > 0){
      meta_df <- meta_df %>% rename(.orientation = direction, .header_group = header_label) %>% select(.header_group,.orientation,data) %>% unnest()
      vars_to_combine <- names(meta_df)[!names(meta_df) %in% c(".header_group",".orientation","col","row")]
      meta_df <- meta_df %>% mutate(value = coalesce(!!!syms(vars_to_combine))) %>% select(-vars_to_combine)
    }
    
    
    
    # Prepare tabledata for joining ----
    tabledata <- tabledata %>% mutate(.header_group = "data", .orientation = "data")
    # Prepare join list ----
    df_list_to_row_bind <- list()
    
    if(length(col_groups) > 0){df_list_to_row_bind <- df_list_to_row_bind %>% append(list(col_groups))}
    if(length(row_groups) > 0){df_list_to_row_bind <- df_list_to_row_bind %>% append(list(row_groups))}
    if(length(meta_df) > 0 & keep_meta_data){df_list_to_row_bind <- df_list_to_row_bind %>% append(list(meta_df))}
    
    to_join <- df_list_to_row_bind  %>% append(list(tabledata)) %>% bind_rows() %>% select(-comment) %>% rename(.value = value)
    
    
    # Join ----
    sheet %>% left_join(to_join)
    
    
  }
