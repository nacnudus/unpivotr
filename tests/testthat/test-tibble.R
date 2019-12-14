context("tibbles")

test_that("functions return tibbles", {
  # Load some pivoted data
  x <- purpose$`up-left left-up`
  # Make a tidy representation
  cells <- as_cells(x)
  cells <- cells[!is.na(cells$chr), ]
  # Select the cells containing the values
  data_cells <-
    cells %>%
    dplyr::filter(row >= 3, col >= 3)
  # Select the column headers
  col_headers <-
    cells %>%
    dplyr::filter(row <= 2) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$row) # Separate each row of headers
  row_headers <-
    cells %>%
    dplyr::filter(col <= 2) %>%
    dplyr::select(row, col, header = chr) %>%
    split(.$col) # Separate each row of headers
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "ABOVE")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "BELOW")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "LEFT")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "RIGHT")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "N")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "E")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "S")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "W")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "NNE")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "NNW")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "ENE")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "ESE")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "SSE")))
  expect_true(tibble::is_tibble(enhead(data_cells, col_headers$`1`, "SSW")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "WSW")))
  expect_true(tibble::is_tibble(enhead(data_cells, row_headers$`1`, "WNW")))
})
