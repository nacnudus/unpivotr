context("tibbles")

test_that("functions return tibbles", {
  # Load some pivoted data
  x <- purpose$`NNW WNW`
  # Make a tidy representation
  cells <- tidy_table(x)
  cells <- cells[!is.na(cells$chr), ]
  # Select the cells containing the values
  datacells <-
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
  expect_true(tibble::is_tibble(ABOVE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(BELOW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(LEFT(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(RIGHT(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(N(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(E(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(S(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(W(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(NNE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(NNW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(ENE(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(ESE(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(SSE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(SSW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(WSW(datacells, row_headers$`1`)))
  expect_true(tibble::is_tibble(WNW(datacells, row_headers$`1`)))
})
