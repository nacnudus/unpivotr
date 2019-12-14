
rm(list = ls())

# Test script 
library("XLConnect")
library(devtools)
library(tidyverse)
load_all("/home/ian/main/projects/unpivotr")

devtools::document()
devtools::build()
devtools::build()

# Check Arrow directions ----------------------------------------------------------------------
direction_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/directions_table.xlsx")

direction_df %>% 
  append_fmt(fmt_font_bold) %>% 
  append_fmt(fmt_font_italic) %>% 
  locate_data(data_type == "numeric") %>% 
  unpivotr::locate_if(fmt_font_bold == T,direction = "WNW",name = WNW) %>% 
  unpivotr::locate_if(fmt_font_bold == F & fmt_font_italic == F,direction = "W",name = W) %>% 
  unpivotr::locate_if(fmt_font_italic == T,direction = "WSW",name = WSW) %>% 
  unpivotr::locate_if(fmt_font_bold == T,direction = "ENE",name = ENE) %>% 
  unpivotr::locate_if(fmt_font_bold == F & fmt_font_italic == F,direction = "E",name = E) %>% 
  unpivotr::locate_if(fmt_font_italic == T,direction = "ESE",name = ESE) %>% 
  unpivotr::locate_if(fmt_font_bold == T,direction = "NNW",name = NNW) %>% 
  unpivotr::locate_if(fmt_font_bold == F & fmt_font_italic == F,direction = "N",name = N) %>% 
  unpivotr::locate_if(fmt_font_italic == T,direction = "NNE",name = NNE) %>% 
  unpivotr::locate_if(fmt_font_bold == T,direction = "SSW",name = SSW) %>% 
  unpivotr::locate_if(fmt_font_bold == F & fmt_font_italic == F,direction = "S",name = S) %>% 
  unpivotr::locate_if(fmt_font_italic == T,direction = "SSE",name = SSE) %>%  
  plot_cells

# Check check hook if  ----------------------------------------------------------------------


direction_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/directions_table.xlsx")

direction_df %>% 
  append_fmt(fmt_font_bold) %>% 
  append_fmt(fmt_font_italic) %>% 
  locate_data(data_type == "numeric") %>% 
  unpivotr::locate_groups(direction = "W",.groupings = groupings(fmt_font_italic,fmt_font_bold),
                          .hook_if = hook(fmt_font_bold ==T), .hook_if_rev = hook(fmt_font_italic == T))  %>% 
  unpivotr::locate_groups(direction = "N",.groupings = groupings(fmt_font_italic,fmt_font_bold),
                          .hook_if = hook(fmt_font_bold ==T), .hook_if_rev = hook(fmt_font_italic == T))  %>% 
  unpivotr::locate_groups(direction = "E",.groupings = groupings(fmt_font_italic,fmt_font_bold),
                          .hook_if = hook(fmt_font_bold ==T), .hook_if_rev = hook(fmt_font_italic == T))  %>% 
  unpivotr::locate_groups(direction = "S",.groupings = groupings(fmt_font_italic,fmt_font_bold),
                          .hook_if = hook(fmt_font_bold ==T), .hook_if_rev = hook(fmt_font_italic == T))  %>% 
  plot_cells()


# Check check tidy abs   ----------------------------------------------------------------------

load_all("/home/ian/main/projects/unpivotr")

tidyabs <-   function(df){

  df_temp <- df %>% append_fmt(fmt_font_bold,fmt_font_italic,fmt_alignment_indent) 
    
  max_indent <- max(df_temp$fmt_alignment_indent, na.rm = T)
  
  df_temp %>% 
      locate_data(data_type == "numeric"| character == "na|\\-") %>% 
      unpivotr::locate_groups(direction = "W",.groupings = groupings(fmt_font_bold,fmt_alignment_indent),
                              .hook_if = hook(any(fmt_font_bold ==T) | any(fmt_alignment_indent < max_indent))) %>%
      unpivotr::locate_groups(direction = "N")
  }
  
tidyabs_ts <-   function(df){
  
  max_header_row <- 
    df %>% filter(data_type == "character") %>% pull(row) %>% max(na.rm = T)
    
  df_temp <- df %>% append_fmt(fmt_font_bold,fmt_font_italic,fmt_alignment_indent) 
  
  max_indent <- max(df_temp$fmt_alignment_indent, na.rm = T)
  
  df_temp %>% 
    locate_data(data_type == "numeric" & row > max_header_row) %>% 
    unpivotr::locate_groups(direction = "W",.groupings = groupings(fmt_font_bold,fmt_alignment_indent),
                            .hook_if = hook(any(fmt_font_bold ==T) | any(fmt_alignment_indent < max_indent))) %>%
    unpivotr::locate_groups(direction = "N")
}


envir_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/environmental-economic-accounts.xlsx",sheets = 2)
envir_df %>% tidyabs() %>% plot_cells()

industry_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/australian-industry.xlsx",sheets = 2)
industry_df %>% tidyabs() %>% plot_cells()

cpi_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/consumer-price-index.xlsx",sheets = 2)
cpi_df %>% tidyabs_ts() %>% plot_cells()


# Check check tidy abs   ----------------------------------------------------------------------

load_all("/home/ian/main/projects/unpivotr")

phd_df <- unpivotr::xlsx_cells_fmt("/home/ian/main/projects/data/PhD_ subfield-citizenship-status-ethnicity-race.xlsx",sheets = 1)

phd_df %>% 
  append_fmt(fmt_font_bold,fmt_font_italic,fmt_alignment_indent) %>% 
  mutate(fmt_alignment_indent_max = max(fmt_alignment_indent)) %>% 
  locate_data(data_type == "numeric") %>% 
  locate_groups(directio= "W", .groupings = groupings(fmt_alignment_indent), 
                .hook_if = hook(any(fmt_alignment_indent < fmt_alignment_indent_max)) ) %>% 
  locate_groups(direction= "N") %>% 
  plot_cells
