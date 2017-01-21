context("join_header()")

library(tidyxl)
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

expect_purpose_short <-
c("0 - 6", "0 - 6", "0 - 6", "7 - 10", "7 - 10", "7 - 10", "0 - 6",
"0 - 6", "0 - 6", "7 - 10", "7 - 10", "7 - 10", "0 - 6", "0 - 6",
"7 - 10", "7 - 10", "7 - 10", "0 - 6", "0 - 6", "0 - 6", "7 - 10",
"7 - 10", "7 - 10", "0 - 6", "0 - 6", "7 - 10", "7 - 10", "7 - 10",
"0 - 6", "0 - 6", "7 - 10", "7 - 10", "7 - 10", "0 - 6", "0 - 6",
"0 - 6", "7 - 10", "7 - 10", "7 - 10", "0 - 6", "0 - 6", "0 - 6",
"7 - 10", "7 - 10", "7 - 10", "0 - 6", "0 - 6", "0 - 6", "7 - 10",
"7 - 10", "7 - 10", "0 - 6", "0 - 6", "7 - 10", "7 - 10")

expect_sex_short <-
c("Female", "Female", "Female", "Female", "Female", "Female",
"Female", "Female", "Female", "Female", "Female", "Female", "Female",
"Female", "Female", "Female", "Female", "Female", "Female", "Female",
"Female", "Female", "Female", "Female", "Female", "Female", "Female",
"Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male",
"Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male",
"Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male",
"Male", "Male", "Male", "Male")

expect_age_short <-
c("15 - 24", "25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64",
"15 - 24", "25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64",
"25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64", "15 - 24",
"25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64", "25 - 44",
"45 - 64", "15 - 24", "25 - 44", "45 - 64", "25 - 44", "45 - 64",
"15 - 24", "25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64",
"15 - 24", "25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64",
"15 - 24", "25 - 44", "45 - 64", "15 - 24", "25 - 44", "45 - 64",
"15 - 24", "25 - 44", "45 - 64", "25 - 44", "45 - 64", "25 - 44",
"45 - 64")

expect_education_short <-
c("Bachelor's degree", "Bachelor's degree", "Bachelor's degree",
"Bachelor's degree", "Bachelor's degree", "Bachelor's degree",
"Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
"Certificate", "Diploma", "Diploma", "Diploma", "Diploma", "Diploma",
"No Qualification", "No Qualification", "No Qualification", "No Qualification",
"No Qualification", "No Qualification", "Postgraduate qualification",
"Postgraduate qualification", "Postgraduate qualification", "Postgraduate qualification",
"Postgraduate qualification", "Bachelor's degree", "Bachelor's degree",
"Bachelor's degree", "Bachelor's degree", "Bachelor's degree",
"Certificate", "Certificate", "Certificate", "Certificate", "Certificate",
"Certificate", "Diploma", "Diploma", "Diploma", "Diploma", "Diploma",
"Diploma", "No Qualification", "No Qualification", "No Qualification",
"No Qualification", "No Qualification", "No Qualification", "Postgraduate qualification",
"Postgraduate qualification", "Postgraduate qualification", "Postgraduate qualification"
)

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

test_that("Compass directions ABOVE and LEFT work", {
  cells <- tidytable(purpose$`ABOVE LEFT`, colnames = FALSE)
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
    ABOVE(col_headers[[1]]) %>%
    LEFT(row_headers[[1]]) %>%
    arrange(row, col)
  expect_equal(datacells[[4]], expect_sex_short)
  expect_equal(datacells[[5]], expect_purpose_short)
})

test_that("Compass directions BELOW and RIGHT work", {
  cells <- tidytable(purpose$`BELOW RIGHT`, colnames = FALSE)
  row_headers <-
    cells %>%
    filter(col >= 7, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <-
    cells %>%
    filter(row >= 11, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <-
    cells %>%
    filter(row <= 10, col <= 6, !is.na(character)) %>%
    select(row, col, value = as.integer(character)) %>%
    BELOW(col_headers[[2]]) %>%
    RIGHT(row_headers[[2]]) %>%
    arrange(row, col)
  expect_equal(datacells[[4]], expect_sex_short)
  expect_equal(datacells[[5]], expect_purpose_short)
})

test_that("Compass directions ABOVE and LEFT work with boundaries", {
  spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
  cells <- tidy_xlsx(spreadsheet, "ABOVE LEFT border")$data[[1]]
  formatting <- tidy_xlsx(spreadsheet)$formats
  left_borders <- which(!is.na(formatting$local$border$left$style))
  top_borders <- which(!is.na(formatting$local$border$top$style))
  left_border_cells <-
    cells %>%
    filter(row == 2, local_format_id %in% left_borders) %>%
    select(row, col)
  top_border_cells <-
    cells %>%
    filter(col == 2, local_format_id %in% top_borders) %>%
    select(row, col)
  row_headers <-
    cells %>%
    filter(col <= 3, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <-
    cells %>%
    filter(row <= 3, !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <-
    cells %>%
    filter(row >= 4, col >= 4, !is.na(content)) %>%
    mutate(content = ifelse(is.na(character), content, NA)) %>%
    select(row, col, value = as.integer(content)) %>%
    ABOVE(col_headers[[1]], left_border_cells) %>% # Different from ABOVE LEFT
    N(col_headers[[2]]) %>% # Same as ABOVE LEFT
    LEFT(row_headers[[1]], top_border_cells) %>% # Different from ABOVE LEFT
    W(row_headers[[2]]) %>% # Same as ABOVE LEFT
    arrange(row, col)
  expect_equal(datacells[[4]], expect_purpose_short)
  expect_equal(datacells[[5]], expect_age_short)
  expect_equal(datacells[[6]], expect_sex_short)
  expect_equal(datacells[[7]], expect_education_short)
  expect_error(ABOVE(datacells, col_headers[[1]], left_border_cells[-2, ]),
               "Multiple headers were detected within the same pair of boundaries.
  Please provide boundaries to separate every header.")
  expect_error(LEFT(datacells, row_headers[[1]], top_border_cells[-2, ]),
               "Multiple headers were detected within the same pair of boundaries.
  Please provide boundaries to separate every header.")
})

test_that("Compass directions BELOW and RIGHT work with boundaries", {
  spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
  cells <- tidy_xlsx(spreadsheet, "BELOW RIGHT border")$data[[1]]
  formatting <- tidy_xlsx(spreadsheet)$formats
  left_borders <- which(!is.na(formatting$local$border$left$style))
  top_borders <- which(!is.na(formatting$local$border$top$style))
  left_border_cells <-
    cells %>%
    filter(row == 14, local_format_id %in% left_borders) %>%
    select(row, col)
  top_border_cells <-
    cells %>%
    filter(col == 11, local_format_id %in% top_borders) %>%
    select(row, col)
  row_headers <-
    cells %>%
    filter(col >= 10, !is.na(content)) %>%
    select(row, col, header = character) %>%
    split(.$col)
  col_headers <-
    cells %>%
    filter(row >= 14, !is.na(content)) %>%
    select(row, col, header = character) %>%
    split(.$row)
  datacells <-
    cells %>%
    filter(row <= 13, col <= 9, !is.na(content)) %>%
    mutate(content = ifelse(is.na(character), content, NA)) %>%
    select(row, col, value = as.integer(content)) %>%
    BELOW(col_headers[[2]], left_border_cells) %>% # Different from BELOW RIGHT
    S(col_headers[[1]]) %>% # Same as BELOW RIGHT
    RIGHT(row_headers[[2]], top_border_cells) %>% # Different from BELOW RIGHT
    E(row_headers[[1]]) %>% # Same as BELOW RIGHT
    arrange(row, col)
  expect_equal(datacells[[4]], expect_purpose_short)
  expect_equal(datacells[[5]], expect_age_short)
  expect_equal(datacells[[6]], expect_sex_short)
  expect_equal(datacells[[7]], expect_education_short)
  expect_error(BELOW(datacells, col_headers[[1]], left_border_cells[-2, ]),
               "Multiple headers were detected within the same pair of boundaries.
  Please provide boundaries to separate every header.")
  expect_error(RIGHT(datacells, row_headers[[1]], top_border_cells[-2, ]),
               "Multiple headers were detected within the same pair of boundaries.
  Please provide boundaries to separate every header.")
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
  expect_error(join_header(datacells, col_headers[[1]], "NORTH"),
               "The direction NORTH, is either not recognised or not yet supported.")
  expect_error(join_header(datacells,
                           col_headers[[1]] %>% extend_S(cells, 1),
                           "W"),
               "Multiple lines of headers are not supported in this way*")
  expect_error(join_header(datacells, col_headers[[1]], "N",
                           boundaries = col_headers[[2]]),
               "'boundaries' is only supported for the directions 'ABOVE', 'RIGHT', 'BELOW' and 'LEFT'.")
  expect_equal(join_header(datacells, col_headers[[1]], "ABOVE"),
               ABOVE(datacells, col_headers[[1]]))
  expect_equal(join_header(datacells, col_headers[[1]], "N"),
               N(datacells, col_headers[[1]]))
})


