context("offset()")

test_that("offset() works", {
  cells <- 
    tidytable(purpose$`NNW WNW`, FALSE, FALSE) %>% 
    dplyr::filter(!is.na(character)) # Introduce 'holes' in the data
  # Select an L-shape with gaps
  bag <- dplyr::filter(cells, row %in% 3:4, col %in% 1:2)
  # Offset and pad (actually anchor) the gaps (now in different parts of the L)
  expect_equal(offset(bag, cells, -1, 1)$character, c(NA, "0 - 6", "7000"))
})
