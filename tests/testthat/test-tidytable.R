context("tidytable()")

library(dplyr)

test_that("Tidytable works with or without row and column names", {
  expect_equal(nrow(tidytable(Formaldehyde)), 20)
  expect_equal(nrow(tidytable(Formaldehyde, rownames = FALSE)), 14)
  expect_equal(nrow(tidytable(Formaldehyde, colnames = FALSE)), 18)
  expect_equal(nrow(tidytable(Formaldehyde, rownames = FALSE, colnames = FALSE)), 12)
})
