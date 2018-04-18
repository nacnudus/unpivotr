context("tibbles")

test_that("functions return tibbles", {
  # Load some pivoted data
  x <- purpose$`NNW WNW`
  # Make a tidy representation
  cells <- tidy_table(x)
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
  expect_true(tibble::is_tibble(ABOVE(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(BELOW(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(LEFT(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(RIGHT(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(N(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(E(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(S(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(W(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(NNE(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(NNW(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(ENE(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(ESE(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(SSE(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(SSW(data_cells, col_headers$`1`)))
  expect_true(tibble::is_tibble(WSW(data_cells, row_headers$`1`)))
  expect_true(tibble::is_tibble(WNW(data_cells, row_headers$`1`)))
})
