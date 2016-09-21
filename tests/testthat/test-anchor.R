context("anchor()")

test_that("a valid cell is returned", {
  cells <- tidytable(purpose$`NNW WNW`)
  expect_error(anchor(cells, 1, 0), "'row' and 'col' must both be >= 1")
  expect_equal(anchor(cells, 1, 1),
               cells[cells$row == 1 & cells$col == 1, ])
  # Check that an invented row is blank besides valid row/col
  pad <- unname(as.list(anchor(cells, 1, 100)))
  expect_equal(unlist(pad[1:2]), c(1, 100))
  expect_true(all(is.na(unlist(pad[-1:-2]))))
})

