#ians-tests

setwd("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")
rm(list = ls())
library("tidyverse")


# Setup packages --------------------------------------------

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
#--------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "humanities"

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
sheet_name <- "performance"

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
sheet_name <- "female"

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
sheet_name <- "male"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats) 

orientated_df_auto %>%  migrate()

#----------------------------------------------------------------------

#----------------------------------------------------------------------

devtools::load_all("C:/Users/Ian/Data/r-projects/unpivotr-dev/unpivotr")

workbook_path <- unpivotr_example("worked-examples.xlsx")
sheet_name <- "implied-multiples"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats,
                 col_header_fill = "style",
                 default_col_header_direction = "NNW") 

orientated_df_auto %>%  migrate() 

#----------------------------------------------------------------------

workbook_path <- unpivotr_example("australian-industry.xlsx") 
sheet_name <-  "Table_1"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)


orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats) 

orientated_df_auto %>% migrate()  %>% View
  

#----------------------------------------------------------------------

workbook_path <- unpivotr_example("consumer-price-index.xlsx") 
sheet_name <-  "Data1"

testsheet_df <-  xlsx_cells(path = workbook_path,sheets = sheet_name)
formats <-  xlsx_formats(workbook_path)

orientated_df_auto <-  
  orientate_auto(sheet = testsheet_df,
                 formats = formats) %>% plot_orientations()

