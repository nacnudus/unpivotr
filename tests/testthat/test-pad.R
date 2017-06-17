context("pad()")

test_that("pad() returns predictable cells", {
  cells <-
    tidy_table(purpose$`NNW WNW`) %>%
    dplyr::filter(!is.na(chr)) # Introduce 'holes' in the data
  # Select a region with gaps
  bag <- dplyr::filter(cells, row %in% 3:4, col %in% 1:2)
  # Pad the gaps
  expect_error(pad(bag, -1), "'rows' and 'cols' must be >= 0")
  expect_identical(pad(bag), pad(bag, 0L, 0L))
  expect_equal(nrow(pad(bag)), 4)
  expect_equal(nrow(pad(bag, 3:5, 1:3)), 9)
  expect_equal(nrow(pad(bag, 6, 4)), 16)
  bag <- bag[FALSE, ]
  expect_identical(pad(bag), bag)
})
