context("offset()")

x <- purpose$`NNW WNW`
cells <- tidy_table(x)
cells <- cells[!is.na(cells$chr), ]

test_that("Offset to non-existant cell works, returning pad", {
  bag <- cells[1, ]
  rowcol <- offset_N(bag, cells[FALSE, ], 1)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 2L, col = 1L))
})

test_that("Offset with zero-row bag is an error", {
  bag <- cells[FALSE, ]
  expect_error(offset_N(bag, cells, 1, include = TRUE),
               "Cannot offset an empty bag \\('bag' has no rows\\)")
})

test_that("Offset to direction other than N, E, S, W is an error", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset(bag, cells, "A", 1),
               "'direction' must be one of 'N', 'S', 'E' and 'W'")
})

test_that("Exactly one of 'n' and 'boundary' must be specified", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells))
  expect_error(offset_N(bag, cells, 2, boundary = ~ TRUE),
               paste0("Exactly one of 'n' and 'boundary' must be specified.\n",
                      "Did you forget the 'cells' argument?"))
})

test_that("'n' must be numeric", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells, ~ TRUE),
               "'n' must be numeric; did you intend to use 'boundary'?")
  expect_error(offset_N(bag, cells, "N"),
               "'n' must be numeric")
})

test_that("'n' must be a whole number", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells, 0.1),
                        "'n' must be a whole number \\(e.g. 1, 1L, 1.0\\)")
})

test_that("'n' must be >= 0", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells, -1), "'n' must be >= 0")
})

test_that("'edge' and 'include' only apply when 'boundary' is specified", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells, edge = TRUE),
               "Exactly one of 'n' and 'boundary' must be specified")
  expect_error(offset_N(bag, cells, include = TRUE),
               "Exactly one of 'n' and 'boundary' must be specified")
})

test_that("Offset by n with edge is an error", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(offset_N(bag, cells, 1, edge = TRUE),
               "'edge' and 'include' only apply when 'boundary' is specified")
  expect_error(offset(bag, cells, "N", 1, edge = TRUE),
               "'edge' and 'include' only apply when 'boundary' is specified")
})

test_that("Offset by n without 'include' or 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- offset_E(bag, cells,
                     boundary = ~ is.na(chr))[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 7L, col = 5:6))
})

test_that("Offset to boundary formula with include works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- offset_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     include = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 6L, col = 1:2))
})

test_that("Offset to boundary formula with 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- offset_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     edge = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 4L, col = 1:2))
})

test_that("Offset to boundary formula with include and 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- offset_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     include = TRUE,
                     edge = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 3L, col = 1:2))
})

test_that("Boundary formulas returning NAs are ignored with a warning", {
  bag <- cells[which(cells$row == 3 & cells$col == 1), ]
  expect_warning(
    rowcol <- offset_E(bag, cells,
                       boundary = ~ as.integer(chr) < 10000
                       )[, c("row", "col")]
    )
  expect_equal(rowcol, data.frame(row = 3L, col = 2L))
})

test_that("All compass directions work", {
  bag <- anchor(cells, 2, 2)
  expect_equal(offset_N(bag, cells, 1)$row, 1)
  expect_equal(offset_E(bag, cells, 1)$col, 3)
  expect_equal(offset_S(bag, cells, 1)$row, 3)
  expect_equal(offset_W(bag, cells, 1)$col, 1)
})

test_that("offset() works with 'n'", {
  bag <- anchor(cells, 2, 2)
  expect_equal(offset(bag, cells, "N", 1)$row, 1)
  expect_equal(offset(bag, cells, "E", 1)$col, 3)
  expect_equal(offset(bag, cells, "S", 1)$row, 3)
  expect_equal(offset(bag, cells, "W", 1)$col, 1)
})

test_that("offset() works with 'boundary'", {
  bag <- anchor(cells, 2, 2)
  expect_equal(offset(bag, cells, "N", boundary = ~ row == 1)$row, 2)
  expect_equal(offset(bag, cells, "E", boundary = ~ col == 3)$col, 2)
  expect_equal(offset(bag, cells, "S", boundary = ~ row == 3)$row, 2)
  expect_equal(offset(bag, cells, "W", boundary = ~ col == 1)$col, 2)
})

test_that("'n' equal to 0 return original bag", {
  bag <- anchor(cells, 2, 2)
  expect_equal(offset_N(bag, cells, n = 0)$row, 2)
})

test_that("Undetected boundaries result in an error", {
  bag <- anchor(cells, 1, 1)
  expect_error(offset_N(bag, cells, boundary = ~ chr == "Boo!"),
               "No boundary detected")
})

test_that("Bags cannot be offseted into zero or negative space", {
  bag <- anchor(cells, 1, 1)
  expect_error(offset_N(bag, cells, 1),
               "The offset went off the edge of the spreadsheet \\(row or col <= 0\\)")
  expect_error(offset_W(bag, cells, 1),
               "The offset went off the edge of the spreadsheet \\(row or col <= 0\\)")
})
