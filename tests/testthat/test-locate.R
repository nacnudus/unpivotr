context("test-locate.R")

test_that("locate_data() works", {
  
    locate_data_test_temp <- 
    unpivotr_example("worked-examples.xlsx") %>% 
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>% 
    attr("data_cells") 
    
  expect_identical(locate_data_test_temp, unpivotr::locate_data_test)
})

test_that("locate() works", {
  
  locate_test_temp <- 
    unpivotr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>% 
    locate(direction = "WNW", name = subject_type) %>% 
    locate(direction = "W", name = subject) %>% 
    locate(direction = "NNW", name = gender) %>% 
    locate(direction = "N", name = name)
  
  expect_identical(locate_test_temp, unpivotr::locate_test)
})

test_that("locate_groups() works", {
  
  locate_groups_test_temp <- 
    unpivotr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
    append_fmt(fmt_alignment_indent) %>% 
    locate_data(data_type == "numeric") %>%
    locate_groups(direction = "W",
                  .groupings = groupings(fmt_alignment_indent),
                  .hook_if =     hook(any(fmt_alignment_indent == 0))) %>%
    locate(direction = "N", name = student) %>% 
    dplyr::select(-character_formatted)
  
  expect_identical(
    locate_groups_test_temp, 
    unpivotr::locate_groups_test %>% 
      dplyr::select(-character_formatted))
})

test_that("migrate() works", {
  
  migrate_test_temp <- 
    unpivotr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>% 
    locate(direction = "WNW", name = subject_type) %>% 
    locate(direction = "W", name = subject) %>% 
    locate(direction = "NNW", name = gender) %>% 
    locate(direction = "N", name = name)  %>% migrate()
  

  expect_identical(migrate_test_temp, unpivotr::migrate_test)
})


