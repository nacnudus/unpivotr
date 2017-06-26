context("extend()")

x <- purpose$`NNW WNW`
cells <- tidy_table(x)
cells <- cells[!is.na(cells$chr), ]

test_that("Extend to non-existant cell works, returning pad", {
  bag <- cells[1, ]
  rowcol <- extend_N(bag, cells[FALSE, ], 1)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 2:3, col = 1L))
})

test_that("Extend with zero-row bag is an error", {
  bag <- cells[FALSE, ]
  expect_error(extend_N(bag, cells, 1, include = TRUE),
               "Cannot extend an empty bag \\('bag' has no rows\\)")
})

test_that("Extend to direction other than N, E, S, W is an error", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend(bag, cells, "A", 1),
               "'direction' must be one of 'N', 'S', 'E' and 'W'")
})

test_that("Exactly one of 'n' and 'boundary' must be specified", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells))
  expect_error(extend_N(bag, cells, 2, boundary = ~ TRUE),
               paste0("Exactly one of 'n' and 'boundary' must be specified.\n",
                      "Did you forget the 'cells' argument?"))
})

test_that("'n' must be numeric", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells, ~ TRUE),
               "'n' must be numeric; did you intend to use 'boundary'?")
  expect_error(extend_N(bag, cells, "N"),
               "'n' must be numeric")
})

test_that("'n' must be a whole number", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells, 0.1),
                        "'n' must be a whole number \\(e.g. 1, 1L, 1.0\\)")
})

test_that("'n' must be >= 0", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells, -1), "'n' must be >= 0")
})

test_that("'edge' and 'include' only apply when 'boundary' is specified", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells, edge = TRUE),
               "Exactly one of 'n' and 'boundary' must be specified")
  expect_error(extend_N(bag, cells, include = TRUE),
               "Exactly one of 'n' and 'boundary' must be specified")
})

test_that("Extend by n with edge is an error", {
  bag <- cells[which(cells$row == 10 & cells$col == 3), ]
  expect_error(extend_N(bag, cells, 1, edge = TRUE),
               "'edge' and 'include' only apply when 'boundary' is specified")
  expect_error(extend(bag, cells, "N", 1, edge = TRUE),
               "'edge' and 'include' only apply when 'boundary' is specified")
})

test_that("Extend by n without 'include' or 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- extend_E(bag, cells,
                     boundary = ~ !is.na(chr))[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = 7L, col = 1:2))
})

test_that("Extend to boundary formula with include works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- extend_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     include = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = c(6L, 6L, 7L, 7L),
                                  col = c(1L, 2L, 1L, 2L)))
})

test_that("Extend to boundary formula with 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- extend_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     edge = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = rep(4:7, each = 2),
                                  col = rep(1:2, times = 4)))
})

test_that("Extend to boundary formula with include and 'edge' works", {
  bag <- cells[which(cells$row == 7 & cells$col %in% 1:2), ]
  rowcol <- extend_N(bag, cells,
                     boundary = ~ !is.na(chr),
                     include = TRUE,
                     edge = TRUE)[, c("row", "col")]
  expect_equal(rowcol, data.frame(row = rep(3:7, each = 2),
                                  col = rep(1:2, times = 5)))
})

test_that("Boundary formulas returning NAs are ignored with a warning", {
  bag <- cells[which(cells$row == 3 & cells$col == 1), ]
  expect_warning(
    rowcol <- extend_E(bag, cells,
                       boundary = ~ as.integer(chr) < 10000
                       )[, c("row", "col")]
    )
  expect_equal(rowcol, data.frame(row = 3L,
                                  col = 2:1))
})

test_that("All compass directions work", {
  bag <- anchor(cells, 2, 2)
  expect_equal(extend_N(bag, cells, 1)$row, 1:2)
  expect_equal(extend_E(bag, cells, 1)$col, 3:2)
  expect_equal(extend_S(bag, cells, 1)$row, 3:2)
  expect_equal(extend_W(bag, cells, 1)$col, 1:2)
})

test_that("extend() works with 'n'", {
  bag <- anchor(cells, 2, 2)
  expect_equal(extend(bag, cells, "N", 1)$row, 1:2)
  expect_equal(extend(bag, cells, "E", 1)$col, 3:2)
  expect_equal(extend(bag, cells, "S", 1)$row, 3:2)
  expect_equal(extend(bag, cells, "W", 1)$col, 1:2)
})

test_that("extend() works with 'boundary'", {
  bag <- anchor(cells, 2, 2)
  expect_equal(extend(bag, cells, "N", boundary = ~ row == 1)$row, 2)
  expect_equal(extend(bag, cells, "E", boundary = ~ col == 3)$col, 2)
  expect_equal(extend(bag, cells, "S", boundary = ~ row == 3)$row, 2)
  expect_equal(extend(bag, cells, "W", boundary = ~ col == 1)$col, 2)
})

test_that("'n' equal to 0 return original bag", {
  bag <- anchor(cells, 2, 2)
  expect_equal(extend_N(bag, cells, n = 0)$row, 2)
})

test_that("Undetected boundaries result in an error", {
  bag <- anchor(cells, 1, 1)
  expect_error(extend_N(bag, cells, boundary = ~ chr == "Boo!"),
               "No boundary detected")
})

test_that("Bags cannot be extended into zero or negative space", {
  bag <- anchor(cells, 1, 1)
  expect_equal(extend_N(bag, cells, 1) %>% nrow, 1)
  expect_equal(extend_W(bag, cells, 1) %>% nrow, 1)
})
