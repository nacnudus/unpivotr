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
    select(row, col, value = character) %>%
    split(.$row)
  row_headers <- 
    cells %>%
    filter(col <= 2, !is.na(character)) %>% # Select all rows of headers at once
    select(row, col, value = character) %>%
    split(.$col) # Return each row of headers in its own element of a list
  datacells <- 
    cells %>%
    filter(row >= 3, col >= 3, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    NNW(col_headers[[1]], "sex") %>%
    N(col_headers[[2]], "purpose") %>%
    WNW(row_headers[[1]], "education") %>% 
    W(row_headers[[2]], "age") %>%
    arrange(row, col)
  expect_equal(datacells$purpose, expect_purpose)
  expect_equal(datacells$sex, expect_sex)
  expect_equal(datacells$age, expect_age)
  expect_equal(datacells$education, expect_education)
})

test_that("Compass directions NNE and WSW work", {
  cells <- tidytable(purpose$`NNE WSW`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col <= 2, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row <= 2, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <- 
    cells %>%
    filter(row >= 3, col >= 3, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    NNE(col_headers[[1]], "sex") %>%
    WSW(row_headers[[1]], "education") %>%
    arrange(row, col)
  expect_equal(datacells$sex, expect_sex)
  expect_equal(datacells$education, expect_education)
})

test_that("Compass directions S, SSE , E and ESE work", {
  cells <- tidytable(purpose$`SSE ESE`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col >= 5, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row >= 21, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$row)
    datacells <- 
    cells %>%
    filter(row <= 20, col <= 4, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    SSE(col_headers[[2]], "sex") %>%
    S(col_headers[[1]], "purpose") %>%
    ESE(row_headers[[2]], "education") %>%
    E(row_headers[[1]], "age") %>%
    arrange(row, col)
  expect_equal(datacells$purpose, expect_purpose)
  expect_equal(datacells$sex, expect_sex)
  expect_equal(datacells$age, expect_age)
  expect_equal(datacells$education, expect_education)
})

test_that("Compass directions SSW and ENE work", {
  cells <- tidytable(purpose$`SSW ENE`, colnames = FALSE)
  row_headers <- 
    cells %>%
    filter(col >= 5, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$col)
  col_headers <- 
    cells %>%
    filter(row >= 21, !is.na(character)) %>%
    select(row, col, value = character) %>%
    split(.$row)
  datacells <- 
    cells %>%
    filter(row <= 20, col <= 4, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    SSW(col_headers[[2]], "sex") %>% # Different from SSE ESE
    ENE(row_headers[[2]], "education") %>% # Different from SSE ESE
    arrange(row, col)
  expect_equal(datacells$sex, expect_sex)
  expect_equal(datacells$education, expect_education)
})

