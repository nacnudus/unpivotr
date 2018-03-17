context("rectify")

x <- data.frame(row = c(1L, 1L, 2L, 2L),
                col = c(1L, 2L, 1L, 2L),
                value = c("a", "b", "c", "d"),
                stringsAsFactors = FALSE)

test_that("rectify() with only the `cells` argument returns zero-dim matrix", {
  expect_equal(rectify(x), matrix(logical(), 0L, 0L)) # type still matters
})

test_that("rectify() puts the cells in the correct places", {
  rectify_correct <- matrix(c(1L, 2L, 1L, 2L),
                            2L, 2L,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, row), rectify_correct)

  rectify_correct <- matrix(c(1L, 1L, 2L, 2L),
                            2L, 2L,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, col), rectify_correct)
})

test_that("rectify() isn't confused by same-named variables in context", {
  value <- "Oops!"
  rectify_correct <- matrix(c("a", "b", "c", "d"),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, value), rectify_correct)
})

test_that("rectify() isn't confused by same-named variables in context", {
  value <- "Oops!"
  rectify_correct <- matrix(c("a", "b", "c", "d"),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, value), rectify_correct)
})

test_that("rectify() handles multiple value columns", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L),
                  col = c(1L, 2L, 1L, 2L),
                  chr = c("a", "b", "c", NA),
                  int = c(NA_integer_, NA_integer_, NA_integer_, 4L),
                  stringsAsFactors = FALSE)
  rectify_correct <- matrix(c("a", "b", "c", NA),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, chr), rectify_correct)
  rectify_correct <- matrix(c(NA_integer_, NA_integer_, NA_integer_, 4L),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, int), rectify_correct)
  rectify_correct <- matrix(c("a", "b", "c", "4"),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, chr, int), rectify_correct)
  expect_equal(rectify(x, chr, int), rectify(x, int, chr)) # vice versa
})


test_that("rectify() only resorts to the character type when necessary", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L),
                  col = c(1L, 2L, 1L, 2L),
                  value1 = c(1, 2, 3, NA),
                  value2 = c(NA, NA, NA, 4))
  rectify_correct <- matrix(c(1, 2, 3, 4),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, value1, value2), rectify_correct)
})

test_that("Blank initial rows and columns are dropped", {
  x <- data.frame(row = c(2L, 2L, 3L, 3L),
                  col = c(2L, 3L, 2L, 3L),
                  value1 = c(1, 2, 3, NA),
                  value2 = c(NA, NA, NA, 4))
  rectify_correct <- matrix(c(1, 2, 3, 4),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(2, 3),
                                            c("2(B)", "3(C)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, value1, value2), rectify_correct)
})
