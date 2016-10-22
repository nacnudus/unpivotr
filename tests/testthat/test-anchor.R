context("anchor()")

test_that("a valid cell is returned", {
  cells <- tidytable(purpose$`NNW WNW`)
  expect_error(anchor(cells, 1, 0),
               "Elements of 'rows' and 'cols' must all be >= 1")
  expect_equal(anchor(cells, 1, 1),
               cells[cells$row == 1 & cells$col == 1, ])
  # Check that an invented row is blank besides valid row/col
  pad <- unname(as.list(anchor(cells, 1, 100)))
  expect_equal(unlist(pad[1:2]), c(1, 100))
  expect_true(all(is.na(unlist(pad[-1:-2]))))
})

test_that("'cross' works", {
  cells <- tidytable(purpose$`NNW WNW`)
  pad <- unname(as.list(anchor(cells, 1:2, 1:3)))
  expect_equal(unlist(pad[1:2]), c(rep(c(1, 2), each = 3), rep(1:3, times = 2)))
  expect_error(anchor(cells, 1:2, 1:3, cross = FALSE),
               "Variables must be length 1 or 3.
Problem variables: 'row'")
})

