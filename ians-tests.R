#ians-tests

setwd("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
rm(list = ls())
library("tidyverse")


# Setup packages --------------------------------------------

write_r_code_text <- function(r_file,file_name){
  r_file %>% read_lines() %>% .[!str_detect(.,"^#") & !str_detect(.,"^[:space:]+#") ]  %>% 
    write_lines(file_name,append = TRUE)
}
prepare_edittable_r_file  <-   function(r_files,file_name){
  file.remove(file_name)  
  
  r_files %>% map(write_r_code_text, file_name = file_name)
  
}

list.files("R",full.names = T) %>% 
  prepare_edittable_r_file(file_name = "./vignettes/all-functions.R")


library("devtools")
library("roxygen2")
library("testthat")
library(magrittr)
library(tidyverse)
library(cellranger)
library(rlang)
library("tidyxl")
library(stringr)

devtools::document("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")



#-------------------------------------------------------------
# Current testing 

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "clean"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats)

orientated_df_auto %>% migrate()

#-------------------------------------------------------------
#-------------------------------------------------------------
# Current testing 
devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "notes"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats)

orientated_df_auto %>% migrate()

#-------------------------------------------------------------
#-------------------------------------------------------------
# Current testing 

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "highlights"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats)

orientated_df_auto %>% migrate()

  orientated_df_auto %>% plot_orientations()

#-------------------------------------------------------------
  #-------------------------------------------------------------
  # Current testing 
  
  devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
  
  workbook_path <- unpivotr_example("worked-examples.xlsx")
  sheet_name <- "annotations"
  
  testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
  formats <-  xlsx_formats(workbook_path)
  
  orientated_df_auto <-  
    orientate_auto(sheet = testsheet_df,
                   formats = formats)
  
  orientated_df_auto %>% migrate()
  
  orientated_df_auto %>% plot_orientations()
  
  #-------------------------------------------------------------
  #-------------------------------------------------------------
  # Current testing 
  
  devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
  
  workbook_path <- unpivotr_example("worked-examples.xlsx")
  sheet_name <- "combined-highlights"
  
  testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
  formats <-  xlsx_formats(workbook_path)
  
  orientated_df_auto <-  
    orientate_auto(sheet = testsheet_df,
                   formats = formats)
  
  orientated_df_auto %>% migrate()
  
  orientated_df_auto %>% plot_orientations()
  
  #-------------------------------------------------------------
  
  #-------------------------------------------------------------
  # Current testing 
  
  devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
  
  workbook_path <- unpivotr_example("worked-examples.xlsx")
  sheet_name <- "highlight-hierarchy"
  
  testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
  formats <-  xlsx_formats(workbook_path)
  
  orientated_df_auto <-  
    orientate_auto(sheet = testsheet_df,
                   formats = formats,group_row_headers_by = "none")

  
    orientated_df_auto %>% migrate()
  

  
  #-------------------------------------------------------------
    #-------------------------------------------------------------
    # Current testing 
    
    devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
    
    workbook_path <- unpivotr_example("worked-examples.xlsx")
    sheet_name <- "sentinels"
    
    testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
    formats <-  xlsx_formats(workbook_path)
    
    orientated_df_auto <-  
      orientate_auto(sheet = testsheet_df,
                     formats = formats,manual_value_references = 'C2:C5')
                        
    orientated_df_auto %>% migrate()
    
    #-------------------------------------------------------------
    
    #-------------------------------------------------------------
    # Current testing 
    
    devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
    
    workbook_path <- unpivotr_example("worked-examples.xlsx")
    sheet_name <- "transposed"
    
    testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
    formats <-  xlsx_formats(workbook_path)
    
    orientated_df_auto <-  
      orientate_auto(sheet = testsheet_df,
                     formats = formats)
    
    orientated_df_auto %>% migrate()
    
    #--------------------------------------------------------------------
    
    devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
    
    workbook_path <- unpivotr_example("worked-examples.xlsx")
    sheet_name <- "pivot-annotations"
    
    testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
    formats <-  xlsx_formats(workbook_path)
    
    
    orientated_df_auto <-  
      orientate_auto(sheet = testsheet_df,
                     formats = formats,
                     col_header_fill = "style",
                     row_header_fill = "style")
    
    orientated_df_auto %>% plot_orientations()
    orientated_df_auto %>% migrate
    
    #----------------------------------------------------------------------
    #--------------------------------------------------------------------
    
    devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
    
    workbook_path <- unpivotr_example("worked-examples.xlsx")
    sheet_name <- "pivot-centre-aligned"
    
    testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
    formats <-  xlsx_formats(workbook_path)
    
    
    orientated_df_auto <-  
      orientate_auto(sheet = testsheet_df,
                     formats = formats,
                     col_header_fill = "border",
                     row_header_fill = "border")
    
    orientated_df_auto %>% migrate 
    
#----------------------------------------------------------------------
    
workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-notes"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                  formats = formats,col_header_fill = "style")

orientated_df_auto %>% plot_orientations()

orientated_df_auto %>% migrate()

#--------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-hierarchy"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats) 

orientated_df_auto %>%  migrate()


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-repeated-headers"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

get_table <- function(x){orientate_auto(sheet = testsheet_df, formats = formats,
               table_range = x,row_header_fill = "style") %>%  migrate}

c("B2:F6,B7:F11,B12:F16,B17:F21")  %>% strsplit(",") %>% unlist %>% 
  map_dfr(get_table )  %>% View


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-header-within-data"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


get_table <- function(x){orientate_auto(sheet = testsheet_df, formats = formats,
                                        table_range = x,col_header_fill = "style") %>%  migrate}

"B2:E7,B8:E13,B14:E19,B20:E25" %>% strsplit(",") %>% unlist %>% 
  map_dfr(get_table )  


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "small-multiples"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


table_ranges = "A1:C4,E1:G4,A6:C9,E6:G9" %>% strsplit(",") %>% unlist
data_ranges = "B3:C4,F3:G4,B8:C9,F8:G9" %>% strsplit(",") %>% unlist

get_table <- function(table, values){
  orientate_auto(sheet = testsheet_df, formats = formats,
                 table_range = table,manual_value_references = values,
                 keep_meta_data = TRUE) %>%  migrate  
}

map2_dfr(table_ranges,data_ranges,get_table)

#----------------------------------------------------------------------




devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-centre-aligned"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 col_header_fill = "border",
                 row_header_fill = "border",
                 group_row_headers_by = c("bolding","italics","indenting","text_color"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>% plot_orientations()


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-centre-aligned"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 col_header_fill = "border",
                 row_header_fill = "border",
                 group_row_headers_by = c("bolding","italics","indenting","text_color"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>% plot_orientations()


#----------------------------------------------------------------------

#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "humanities"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 group_row_headers_by = c("indenting"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>%  migrate()

orientated_df_auto %>% plot_orientations()


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "female"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 group_row_headers_by = c("indenting"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>%  migrate()

orientated_df_auto %>% plot_orientations()


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "male"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 group_row_headers_by = c("indenting"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>%  migrate()

orientated_df_auto %>% plot_orientations()


#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "implied-multiples"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 col_header_fill = "style",
                 group_row_headers_by = c("indenting"),
                 group_col_headers_by = c("bolding"),
                 default_col_header_direction = "NNW") 

orientated_df_auto %>%  migrate() 

#----------------------------------------------------------------------
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "non-text headers"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 col_header_fill = "style",
                 group_row_headers_by = c("indenting"),
                 group_col_headers_by = c("bolding")) 

orientated_df_auto %>%  migrate() 

#----------------------------------------------------------------------














# Add functions --------------------------------------------
#get_range_df
# source("all-functions.R")


workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "pivot-notes"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientations <- c("D3:G3" = "N", "F2,D2" = "NNW", "C4:C7" = "W", 
                  "B5,B7" = "W", "D4:G7" = "data", "B4,B6" = "WNW")


orientated_df <- orientate(testsheet_df,orientations)
orientated_df %>% plot_orientations()

auto_orientated_df <- orientate_auto(testsheet_df,formats)
auto_orientated_df %>% plot_orientations()




testsheet_df %>% group_by(ro)




borders_df <- 
testsheet_df %>% 
  mutate(left_border =  local_format_id %>% map_lgl(~formats$local$border$right$style[[.x]] != "")) %>%
  mutate(right_border = local_format_id %>% map_lgl(~formats$local$border$left$style[[.x]] != "")) %>% 
  select(address,row,col,left_border,right_border) 

left_borders <- borders_df$col[borders_df$left_border] %>% .[!is.na(.)] %>% unique 
right_borders <- borders_df$col[borders_df$right_border] %>% .[!is.na(.)] %>% unique %>% `+`(.,1)

border <- c(right_borders,left_borders) %>% unique

borders_df <- 
  tibble(min_col = c(0,border)) %>% 
    mutate(max_col = lead(min_col,1) - 1) %>% 
    mutate(max_col = ifelse(is.na(max_col), 100,max_col)) %>% 
    mutate(border_group = row_number()) %>% 
    mutate(col = map2(min_col,max_col,function(x,y){c(x:y)}))

border_join <-
  borders_df %>% 
    select(border_group, col) %>% 
    unnest()

testsheet_df %>% left_join(border_join,by = "col")



auto_orientated_df %>% glimpse



orientated_df  %>% migrate() %>% 
  mutate_at(.vars = vars(matches("row_")),
            .funs = funs( ifelse(is.na(.),
                                 NA,
                                 paste(str_pad(as.character(row),width = 5,pad = "0"),
                                       .,
                                       sep = "_")))) %>%
  mutate( cols = paste3(str_pad(as.character(col),width = 5,pad = "0"), !!!syms(names(.)[str_detect(names(.),"col_")]), sep = "_")) %>%
  select(-row,-col) %>%
  select(-matches("col_"))  %>%
  mutate(value = as.numeric(.value)) %>%
  spread(cols,value) %>%
  select(everything(), sort(names(.)[str_detect(names(.),"^[0-9]{5}")])) %>%
  set_names(stringr::str_replace(names(.),"^[0-9]{5}_","")) %>%
  arrange(!!!syms(names(.)[str_detect(names(.),"row_")])) %>%
  mutate_at(.vars = vars(matches("row_")),.funs = funs( ifelse(is.na(.), NA, str_replace_all(.,"[0-9]{5}_",""))))



auto_orientated_df %>% migrate()


#-----------------------------------------------------------




orientated_df %>% glimpse
orientated_df %>% filter(.orientation == "data") %>% glimpse


(tidy_df <- migrate(orientated_df))





header_df <-
  sheet %>%
  # filter(!is_blank) %>%
  filter(col <= value_ref$max_col) %>%
  filter(col >= value_ref$min_col) %>%
  filter(row < value_ref$min_row) %>%
  mutate(row_temp = row)


testsheet_df %>% add_h_border_groups(formats)


testsheet_df %>%  
  add_v_border_groups(formats) %>%
  add_h_border_groups(formats) %>% 
  ggplot(aes(y = -row,  x = col, fill = v_border_group)) + 
  geom_tile(colour = "black")

  
testsheet_df %>%  
  add_h_border_groups(formats) %>% 
  group_by(h_border_group)  %>%  
  select(row,col,h_border_group, character) %>% 
  mutate(value = paste3(character, collapse = " _ ") %>% str_remove(" _ $"))



paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}












