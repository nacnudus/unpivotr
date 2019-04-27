
create_h_border_groups <- function(df,formats){
  
  df %>% 
    select(address, row_temp, col, numeric, character)
  
  
  borders_df <- 
    df %>% 
    mutate(left_border =  local_format_id %>% map_lgl(~formats$local$border$right$style[[.x]] != "")) %>%
    mutate(right_border = local_format_id %>% map_lgl(~formats$local$border$left$style[[.x]] != "")) %>% 
    select(address,row_temp,col,left_border,right_border) 
  
  left_borders <- borders_df$col[borders_df$left_border] %>% .[!is.na(.)] %>% unique 
  right_borders <- borders_df$col[borders_df$right_border] %>% .[!is.na(.)] %>% unique %>% `+`(.,1)
  
  border <- c(right_borders,left_borders) %>% unique
  
  current_row <- df$row_temp %>% unique
  
  borders_df <- 
    tibble(max_col = border) %>% 
    mutate(min_col = lag(max_col,1) +1) %>% 
    mutate(min_col = ifelse(row_number() == 1, 0, min_col)) %>% 
    bind_rows(.,tibble(max_col = 1000, min_col = max(.$max_col))+1 ) %>% 
    mutate(h_border_group =paste0(current_row,"_", as.character(row_number()))) %>% 
    mutate(col = map2(min_col,max_col,function(x,y){c(x:y)}))
  
  border_join <-
    borders_df %>% 
    select(h_border_group, col) %>% 
    unnest()
  
  df %>% left_join(border_join,by = "col") %>% 
    select(row =  row_temp, col,h_border_group)
}
add_h_border_groups <- function(sheet,formats){
  sheet_01 <- 
    sheet %>% 
    mutate(row_temp = row ) %>% 
    group_by(row) %>% 
    nest()
  
  df <- sheet_01$data[[1]]
  
  bgs <- sheet_01$data %>% map(create_h_border_groups,formats = formats) %>% bind_rows()
  
  sheet  %>% left_join(bgs, by = c("row", "col")) 
}


add_v_border_groups <- function(sheet,formats){
  
  
  sheet_01 <- 
    sheet %>% 
    mutate(col_temp = col ) %>% 
    group_by(col) %>% 
    nest()
  
  df <- sheet_01$data[[1]]
  
  bgs <- sheet_01$data %>% map(create_v_border_groups, formats = formats) %>% bind_rows()
  
  sheet  %>% left_join(bgs, by = c("row", "col")) 
}
create_v_border_groups <- function(df,formats){
  
  
  
  df %>% 
    select(address, col_temp, row, numeric, character)
  
  
  
  borders_df <- 
    df %>% 
    mutate(top_border =  local_format_id %>% map_lgl(~formats$local$border$top$style[[.x]] != "")) %>%
    mutate(bottom_border = local_format_id %>% map_lgl(~formats$local$border$bottom$style[[.x]] != "")) %>% 
    select(address,col_temp,row,top_border,bottom_border) 
  
  top_borders <- borders_df$row[borders_df$top_border] %>% .[!is.na(.)] %>% unique 
  bottom_borders <- borders_df$row[borders_df$bottom_border] %>% .[!is.na(.)] %>% unique %>% `+`(.,1)
  
  border <- c(top_borders,bottom_borders) %>% unique
  
  current_col <- df$col_temp %>% unique
  
  borders_df <- 
    tibble(min_row = border) %>% 
    mutate(max_row = lead(min_row,1) -1) %>% 
    mutate(max_row = ifelse(row_number() == nrow(.), 1000, max_row)) %>% 
    bind_rows(tibble(max_row = min(.$min_row)-1, min_row = 0 ),.) %>% 
    mutate(v_border_group =paste0(current_col,"_", as.character(row_number()))) %>% 
    mutate(row = map2(min_row,max_row,function(x,y){c(x:y)}))
  
  border_join <-
    borders_df %>% 
    select(v_border_group, row) %>% 
    unnest()
  
  df %>% left_join(border_join,by = "row") %>% 
    select(col =  col_temp, row,v_border_group)
}






