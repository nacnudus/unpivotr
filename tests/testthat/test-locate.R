context("Locate")

testthat::expect_true(
  label = "Locate Data",
  info = "Data cells were not located correctly.",
  object = {
    
    worked.example_data <- 
      unpivotr_example("worked-examples.xlsx") %>% 
        xlsx_cells_fmt(sheets = "pivot-annotations") %>%
        locate_data(data_type == "numeric") 
    
  
    identical(worked.example_data, worked_example_datacells)
          
  })
  

testthat::expect_true(
  label = "Locate_if headers",
  info = "Headers were not located correctly with `locate_if`.",
  object = {

    worked.example_locate_if <- 
      unpivotr_example("worked-examples.xlsx") %>% 
        xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
        append_fmt(fmt_alignment_indent) %>%
        locate_data(data_type == "numeric") %>%
        locate_if(fmt_alignment_indent == 0, direction = "WNW", name = subject_type) %>% 
        locate_if(fmt_alignment_indent == 1, direction = "W", name = subject) %>% 
        locate(direction = "N", name = student)
    
    identical(worked.example_locate_if,worked_example_locate_if)    
        
  })    
    
    
    