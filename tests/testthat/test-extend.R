context("extend()")

x <- purpose$`NNW WNW`
cells <- tidytable(x, rownames = FALSE, colnames = FALSE)
cells <- cells[!is.na(cells$character), ]
cell <- cells[which(cells$row == 10 & cells$col == 3), ]

test_that("Extend to boundary blank works", {
  rowcol <- extend_N(cell, cells, boundary = "blank")[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = c(9L, 8L, 7L, 10L), col = 3L))
})

test_that("Extend to boundary formula and include works", {
  rowcol <- extend_E(cell, cells,
                     boundary = ~ col == 5,
                     include = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 10L, col = c(4L, 5L, 3L)))
})

test_that("Boundary formulas returning NAs are handled correctly", {
  rowcol <- extend_N(cell, cells,
                     boundary = ~ as.integer(character) < 10000)$row %>% sort
  expect_equal(rowcol, c(4:5, 7:10))
})

