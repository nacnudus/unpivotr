context("join_header()")

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
  cells <- tidy_table(purpose$`NNW WNW`)
  col_headers <-
    cells %>%
    dplyr::filter(row <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  row_headers <-
    cells %>%
    dplyr::filter(col <= 2, !is.na(chr)) %>% # Select all rows of headers at once
    dplyr::select(row, col, header = chr) %>%
    split(.$col) # Return each row of headers in its own element of a list
  datacells <-
    cells %>%
    dplyr::filter(row >= 3, col >= 3, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[1]], "NNW") %>%
    enhead(col_headers[[2]], "N") %>%
    enhead(row_headers[[1]], "WNW") %>%
    enhead(row_headers[[2]], "W") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[4]], expect_sex)
  expect_equal(datacells[[5]], expect_purpose)
  expect_equal(datacells[[6]], expect_education)
  expect_equal(datacells[[7]], expect_age)
})

test_that("Compass directions NNE and WSW work", {
  cells <- tidy_table(purpose$`NNE WSW`)
  row_headers <-
    cells %>%
    dplyr::filter(col <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row >= 3, col >= 3, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[1]], "NNE") %>%
    enhead(row_headers[[1]], "WSW") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[5]], expect_education)
  expect_equal(datacells[[4]], expect_sex)
})

test_that("Compass directions S, SSE , E and ESE work", {
  cells <- tidy_table(purpose$`SSE ESE`)
  row_headers <-
    cells %>%
    dplyr::filter(col >= 5, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row >= 21, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
    datacells <-
    cells %>%
    dplyr::filter(row <= 20, col <= 4, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[2]], "SSE") %>%
    enhead(col_headers[[1]], "S") %>%
    enhead(row_headers[[2]], "ESE") %>%
    enhead(row_headers[[1]], "E") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[4]], expect_sex)
  expect_equal(datacells[[5]], expect_purpose)
  expect_equal(datacells[[6]], expect_education)
  expect_equal(datacells[[7]], expect_age)
})

test_that("Compass directions SSW and ENE work", {
  cells <- tidy_table(purpose$`SSW ENE`)
  row_headers <-
    cells %>%
    dplyr::filter(col >= 5, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row >= 21, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row <= 20, col <= 4, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[2]], "SSW") %>%
    enhead(row_headers[[2]], "ENE") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[5]], expect_education)
  expect_equal(datacells[[4]], expect_sex)
})

test_that("Compass directions ABOVE and LEFT work", {
  cells <- tidy_table(purpose$`ABOVE LEFT`)
  row_headers <-
    cells %>%
    dplyr::filter(col <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row >= 3, col >= 3, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[1]], "ABOVE", drop = FALSE) %>%
    enhead(row_headers[[1]], "LEFT", drop = FALSE) %>%
    enhead(col_headers[[2]], "ABOVE", drop = FALSE) %>%
    enhead(row_headers[[2]], "LEFT", drop = FALSE) %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[5]], expect_sex_short)
  expect_equal(datacells[[4]], expect_purpose_short)
})

test_that("Compass directions BELOW and RIGHT work", {
  cells <- tidy_table(purpose$`BELOW RIGHT`)
  row_headers <-
    cells %>%
    dplyr::filter(col >= 7, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row >= 11, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row <= 10, col <= 6, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value) %>%
    BELOW(col_headers[[2]]) %>%
    RIGHT(row_headers[[2]]) %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[5]], expect_sex_short)
  expect_equal(datacells[[4]], expect_purpose_short)
})

test_that("Compass directions ABOVE and LEFT work with corner_cells", {
  spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
  cells <- tidyxl::xlsx_cells(spreadsheet, "ABOVE LEFT border")
  formatting <- tidyxl::xlsx_formats(spreadsheet)
  left_borders <- which(!is.na(formatting$local$border$left$style))
  top_borders <- which(!is.na(formatting$local$border$top$style))
  left_border_cells <-
    cells %>%
    dplyr::filter(row == 2, local_format_id %in% left_borders) %>%
    dplyr::select(row, col)
  top_border_cells <-
    cells %>%
    dplyr::filter(col == 2, local_format_id %in% top_borders) %>%
    dplyr::select(row, col)
  row_headers <-
    cells %>%
    dplyr::filter(col <= 3, !is.na(character)) %>%
    dplyr::select(row, col, header = character) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row <= 3, !is.na(character)) %>%
    dplyr::select(row, col, header = character) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row >= 4, col >= 4, !is_blank) %>%
    dplyr::mutate(content = ifelse(is.na(character), numeric, NA)) %>%
    dplyr::mutate(value = as.integer(content)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[1]], "ABOVE", left_border_cells) %>%
    enhead(col_headers[[2]], "N") %>%
    enhead(row_headers[[1]], "LEFT", top_border_cells) %>%
    enhead(row_headers[[2]], "W") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[4]], expect_purpose_short)
  expect_equal(datacells[[5]], expect_age_short)
  expect_equal(datacells[[6]], expect_sex_short)
  expect_equal(datacells[[7]], expect_education_short)
  expect_error(enhead(datacells,
                      col_headers[[1]],
                      "ABOVE",
                      left_border_cells[-2, ]),
               "`corner_cells` must have the same number of rows as `header_cells`.")
  expect_error(enhead(datacells,
                      row_headers[[1]],
                      "LEFT",
                      top_border_cells[-2, ]),
               "`corner_cells` must have the same number of rows as `header_cells`.")
})

test_that("Compass directions BELOW and RIGHT work with corner_cells", {
  spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
  cells <- tidyxl::xlsx_cells(spreadsheet, "BELOW RIGHT border")
  formatting <- tidyxl::xlsx_formats(spreadsheet)
  left_borders <- which(!is.na(formatting$local$border$left$style))
  top_borders <- which(!is.na(formatting$local$border$top$style))
  left_border_cells <-
    cells %>%
    dplyr::filter(row == 14, local_format_id %in% left_borders) %>%
    dplyr::select(row, col)
  top_border_cells <-
    cells %>%
    dplyr::filter(col == 11, local_format_id %in% top_borders) %>%
    dplyr::select(row, col)
  row_headers <-
    cells %>%
    dplyr::filter(col >= 10, !is_blank) %>%
    dplyr::select(row, col, header = character) %>%
    split(.$col)
  col_headers <-
    cells %>%
    dplyr::filter(row >= 14, !is_blank) %>%
    dplyr::select(row, col, header = character) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row <= 13, col <= 9, !is_blank) %>%
    dplyr::mutate(content = ifelse(is.na(character), numeric, NA)) %>%
    dplyr::mutate(value = as.integer(content)) %>%
    dplyr::select(row, col, value) %>%
    enhead(col_headers[[2]], "BELOW", left_border_cells) %>%
    enhead(col_headers[[1]], "S") %>%
    enhead(row_headers[[2]], "RIGHT", top_border_cells, "") %>%
    enhead(row_headers[[1]], "E") %>%
    dplyr::arrange(row, col)
  expect_equal(datacells[[4]], expect_purpose_short)
  expect_equal(datacells[[5]], expect_age_short)
  expect_equal(datacells[[6]], expect_sex_short)
  expect_equal(datacells[[7]], expect_education_short)
  expect_error(enhead(datacells,
                      col_headers[[1]],
                      "BELOW",
                      left_border_cells[-2, ]),
               "`corner_cells` must have the same number of rows as `header_cells`.")
  expect_error(enhead(datacells,
                      row_headers[[1]],
                      "RIGHT",
                      top_border_cells[-2, ]),
               "`corner_cells` must have the same number of rows as `header_cells`.")
})

test_that("enhead() works", {
  cells <- tidy_table(purpose$`NNW WNW`)
  col_headers <-
    cells %>%
    dplyr::filter(row <= 2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row)
  datacells <-
    cells %>%
    dplyr::filter(row >= 3, col >= 3, !is.na(chr)) %>%
    dplyr::mutate(value = as.integer(chr)) %>%
    dplyr::select(row, col, value)
  multirow_header <-
    cells %>%
    dplyr::filter(row %in% 1:2, !is.na(chr)) %>%
    dplyr::select(row, col, header = chr)
  expect_error(enhead(datacells, col_headers[[1]], "NORTH"),
               "`direction` must be one of \"NNW\", \"N\", \"NNE\", \"ENE\", \"E\", \"ESE\", \"SSE\", \"S\", \"SSW\", \"WSW\", \"W\", \"WNW\", \"ABOVE\", \"LEFT\", \"RIGHT\", \"BELOW\"")
  expect_error(enhead(datacells, multirow_header, "W"),
               "Multiple lines of headers are not supported in this way*")
  expect_error(enhead(datacells, col_headers[[1]], "N",
                           corner_cells = col_headers[[2]]),
               "'corner_cells' is only supported for the directions 'ABOVE', 'RIGHT', 'BELOW' and 'LEFT'.")
})

test_that("the `drop` argument works", {
  spreadsheet <- system.file("extdata/purpose.xlsx", package = "unpivotr")
  cells <- tidyxl::xlsx_cells(spreadsheet, "ABOVE LEFT border")
  formatting <- tidyxl::xlsx_formats(spreadsheet)
  left_borders <- which(!is.na(formatting$local$border$left$style))
  top_borders <- which(!is.na(formatting$local$border$top$style))
  left_border_cells <-
    dplyr::filter(cells, row == 2, local_format_id %in% left_borders) %>%
    dplyr::select(row, col)
  top_border_cells <-
    dplyr::filter(cells, col == 2, local_format_id %in% top_borders) %>%
    dplyr::select(row, col)
  sex <-
    dplyr::filter(cells, col == 2, !is_blank) %>%
    dplyr::select(row, col, sex = character)
  qualification <-
    dplyr::filter(cells, col == 3, !is_blank) %>%
    dplyr::select(row, col, qualification = character)
  satisfaction <-
    dplyr::filter(cells, row == 2, !is_blank) %>%
    dplyr::select(row, col, satisfaction = character)
  age <-
    dplyr::filter(cells, row == 3, !is_blank) %>%
    dplyr::select(row, col, age = character)
  datacells <-
    dplyr::filter(cells, row >= 4, col >= 4, !is_blank) %>%
    dplyr::transmute(row, col, value = as.integer(numeric))
  expect_equal(nrow(enhead(datacells, satisfaction, "N")), 20)
  expect_equal(nrow(enhead(datacells, satisfaction, "N", drop = FALSE)), 55)
  expect_equal(nrow(enhead(datacells, sex, "W")), 12)
  expect_equal(nrow(enhead(datacells, sex, "W", drop = FALSE)), 55)
  expect_equal(nrow(enhead(datacells, satisfaction, "NNW")), 39)
  expect_equal(nrow(enhead(datacells, satisfaction, "NNW", drop = FALSE)), 55)
  expect_equal(nrow(enhead(datacells, sex, "WNW")), 49)
  expect_equal(nrow(enhead(datacells, sex, "WNW", drop = FALSE)), 55)
})
