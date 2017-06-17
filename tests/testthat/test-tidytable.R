context("tidy_table()")

library(dplyr)

test_that("tidy_table works with or without row and column names", {
  expect_equal(nrow(tidy_table(Formaldehyde)), 12)
  expect_equal(nrow(tidy_table(Formaldehyde, colnames = TRUE)), 14)
  expect_equal(nrow(tidy_table(Formaldehyde, rownames = TRUE)), 18)
  expect_equal(nrow(tidy_table(Formaldehyde, rownames = TRUE, colnames = TRUE)), 20)
})
