context("join_header()")

library(dplyr)

expect_purpose <- 
c("0 - 6", "7 - 10", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", 
"0 - 6", "7 - 10", "0 - 6", "7 - 10", "7 - 10", "0 - 6", "7 - 10", 
"0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", 
"7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", 
"0 - 6", "7 - 10", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", 
"0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", 
"7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", 
"0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", 
"7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "7 - 10", "0 - 6", 
"7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", "0 - 6", "7 - 10", 
"7 - 10", "7 - 10")

expect_sex <- 
c("Female", "Female", "Male", "Female", "Female", "Male", "Male", 
"Female", "Female", "Male", "Male", "Female", "Male", "Male", 
"Female", "Female", "Male", "Male", "Female", "Female", "Male", 
"Male", "Female", "Female", "Male", "Male", "Female", "Female", 
"Male", "Male", "Female", "Male", "Male", "Female", "Female", 
"Male", "Male", "Female", "Female", "Male", "Male", "Female", 
"Female", "Male", "Male", "Female", "Female", "Male", "Male", 
"Female", "Female", "Male", "Male", "Female", "Female", "Male", 
"Male", "Female", "Female", "Male", "Male", "Female", "Female", 
"Female", "Male", "Male", "Female", "Female", "Male", "Male", 
"Female", "Male")

expect_age <- 
c("15 - 24", "15 - 24", "15 - 24", "25 - 44", "25 - 44", "25 - 44", 
"25 - 44", "45 - 64", "45 - 64", "45 - 64", "45 - 64", "65+", 
"65+", "65+", "15 - 24", "15 - 24", "15 - 24", "15 - 24", "25 - 44", 
"25 - 44", "25 - 44", "25 - 44", "45 - 64", "45 - 64", "45 - 64", 
"45 - 64", "65+", "65+", "65+", "65+", "15 - 24", "15 - 24", 
"15 - 24", "25 - 44", "25 - 44", "25 - 44", "25 - 44", "45 - 64", 
"45 - 64", "45 - 64", "45 - 64", "65+", "65+", "65+", "65+", 
"15 - 24", "15 - 24", "15 - 24", "15 - 24", "25 - 44", "25 - 44", 
"25 - 44", "25 - 44", "45 - 64", "45 - 64", "45 - 64", "45 - 64", 
"65+", "65+", "65+", "65+", "15 - 24", "25 - 44", "25 - 44", 
"25 - 44", "25 - 44", "45 - 64", "45 - 64", "45 - 64", "45 - 64", 
"65+", "65+")

expect_education <- 
c("Bachelor's degree", "Bachelor's degree", "Bachelor's degree", 
"Bachelor's degree", "Bachelor's degree", "Bachelor's degree", 
"Bachelor's degree", "Bachelor's degree", "Bachelor's degree", 
"Bachelor's degree", "Bachelor's degree", "Bachelor's degree", 
"Bachelor's degree", "Bachelor's degree", "Certificate", "Certificate", 
"Certificate", "Certificate", "Certificate", "Certificate", "Certificate", 
"Certificate", "Certificate", "Certificate", "Certificate", "Certificate", 
"Certificate", "Certificate", "Certificate", "Certificate", "Diploma", 
"Diploma", "Diploma", "Diploma", "Diploma", "Diploma", "Diploma", 
"Diploma", "Diploma", "Diploma", "Diploma", "Diploma", "Diploma", 
"Diploma", "Diploma", "No Qualification", "No Qualification", 
"No Qualification", "No Qualification", "No Qualification", "No Qualification", 
"No Qualification", "No Qualification", "No Qualification", "No Qualification", 
"No Qualification", "No Qualification", "No Qualification", "No Qualification", 
"No Qualification", "No Qualification", "Postgraduate qualification", 
"Postgraduate qualification", "Postgraduate qualification", "Postgraduate qualification", 
"Postgraduate qualification", "Postgraduate qualification", "Postgraduate qualification", 
"Postgraduate qualification", "Postgraduate qualification", "Postgraduate qualification", 
"Postgraduate qualification")

test_that("Compass directions N, NNW, W, and WNW work", {
  cells <- tidytable(purpose$`NNW WNW`, colnames = FALSE)
  col_headers <- 
    cells %>%
    filter(row <= 2, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  row_headers <- 
    cells %>%
    filter(col <= 2, !is.na(character)) %>% # Select all rows of headers at once
    select(row, col, header = character) %>%
    split(.$col) # Return each row of headers in its own element of a list
  datacells <- 
    cells %>%
    filter(row >= 3, col >= 3, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    NNW(col_headers[[1]]) %>%
    N(col_headers[[2]]) %>%
    WNW(row_headers[[1]]) %>% 
    W(row_headers[[2]]) %>%
    arrange(row, col)
  expect_equal(datacells[[4]], expect_sex)
  expect_equal(datacells[[5]], expect_purpose)
  expect_equal(datacells[[6]], expect_education)
  expect_equal(datacells[[7]], expect_age)
})

test_that("Compass directions NNE and WSW work", {
  cells <- tidytable(purpose$`NNE WSW`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col <= 2, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row <= 2, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <- 
    cells %>%
    filter(row >= 3, col >= 3, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    NNE(col_headers[[1]]) %>%
    WSW(row_headers[[1]]) %>%
    arrange(row, col)
  expect_equal(datacells[[4]], expect_education)
  expect_equal(datacells[[5]], expect_sex)
})

test_that("Compass directions S, SSE , E and ESE work", {
  cells <- tidytable(purpose$`SSE ESE`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col >= 5, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row >= 21, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
    datacells <- 
    cells %>%
    filter(row <= 20, col <= 4, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    SSE(col_headers[[2]]) %>%
    S(col_headers[[1]]) %>%
    ESE(row_headers[[2]]) %>%
    E(row_headers[[1]]) %>%
    arrange(row, col)
  expect_equal(datacells[[4]], expect_sex)
  expect_equal(datacells[[5]], expect_purpose)
  expect_equal(datacells[[6]], expect_education)
  expect_equal(datacells[[7]], expect_age)
})

test_that("Compass directions SSW and ENE work", {
  cells <- tidytable(purpose$`SSW ENE`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col >= 5, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row >= 21, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <- 
    cells %>%
    filter(row <= 20, col <= 4, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    SSW(col_headers[[2]]) %>% # Different from SSE ESE
    ENE(row_headers[[2]]) %>% # Different from SSE ESE
    arrange(row, col)
  expect_equal(datacells[[4]], expect_education)
  expect_equal(datacells[[5]], expect_sex)
})

test_that("join_header() works", {
  cells <- tidytable(purpose$`NNW WNW`, colnames = FALSE)
  col_headers <- 
    cells %>%
    filter(row <= 2, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <- 
    cells %>%
    filter(row >= 3, col >= 3, !is.na(character)) %>%
    select(row, col, value = as.integer(character))
  expect_error(join_header(datacells, col_headers[[1]], "NORTH"))
  expect_error(join_header(datacells, 
                           col_headers[[1]] %>% extend_S(cells, 1),
                           "W"))
  expect_error(join_header(datacells, col_headers[[1]], "N", 
                           boundaries = col_headers[[2]]))
  expect_equal(join_header(datacells, col_headers[[1]], "ABOVE"),
               ABOVE(datacells, col_headers[[1]]))
  expect_equal(join_header(datacells, col_headers[[1]], "N"),
               N(datacells, col_headers[[1]]))
})


