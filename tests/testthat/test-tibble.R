context("tibbles")

test_that("functions return tibbles", {
  library(dplyr)
  # Load some pivoted data
  x <- purpose$`NNW WNW`
  # Make a tidy representation
  cells <- tidy_table(x)
  cells <- cells[!is.na(cells$chr), ]
  # Select the cells containing the values
  datacells <-
    cells %>%
    filter(row >= 3, col >= 3)
  # Select the column headers
  col_headers <-
    cells %>%
    filter(row <= 2) %>%
    select(row, col, header = chr) %>%
    split(.$row) # Separate each row of headers
  expect_true(tibble::is_tibble(pad(datacells)))
  expect_true(tibble::is_tibble(anchor(cells, 1, 1)))
  expect_true(tibble::is_tibble(ABOVE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(BELOW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(LEFT(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(RIGHT(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(N(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(E(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(S(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(W(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(NNE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(NNW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(ENE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(ESE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(SSE(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(SSW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(WSW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(WNW(datacells, col_headers$`1`)))
  expect_true(tibble::is_tibble(extend(datacells, cells, "N", 1)))
  expect_true(tibble::is_tibble(extend(datacells, cells, "N", boundary = ~ TRUE)))
})


