context("pad()")

test_that("pad() returns predictable cells", {
  cells <- 
    tidytable(purpose$`NNW WNW`, FALSE, FALSE) %>% 
    dplyr::filter(!is.na(character)) # Introduce 'holes' in the data
  # Select a region with gaps
  bag <- dplyr::filter(cells, row %in% 3:4, col %in% 1:2)
  # Pad the gaps
  expect_error(pad(bag, -1), "'rows' and 'cols' must be >= 0")
  expect_equal(pad(bag), pad(bag, 0L, 0L))
  expect_equal(nrow(pad(bag)), 4)
  expect_equal(nrow(pad(bag, 3:5, 1:3)), 9)
  expect_equal(nrow(pad(bag, 6, 4)), 16)
  bag <- bag[FALSE, ]
  expect_equal(pad(bag), bag)
})
