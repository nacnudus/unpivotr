context("rectify")

test_that("rectify() puts the cells in the correct places", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L),
                  col = c(1L, 2L, 1L, 2L),
                  data_type = rep("character", 4),
                  character = c("a", "b", "c", "d"),
                  stringsAsFactors = FALSE)
  rectify_correct <- matrix(as.character(c(1L, 2L, 1L, 2L)),
                            2L, 2L,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, row), rectify_correct)
  rectify_correct <- matrix(as.character(c(1L, 1L, 2L, 2L)),
                            2L, 2L,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, col), rectify_correct)
})

test_that("rectify() isn't confused by same-named variables in context", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L),
                  col = c(1L, 2L, 1L, 2L),
                  data_type = rep("character", 4),
                  character = c("a", "b", "c", "d"),
                  stringsAsFactors = FALSE)
  data_type <- "Oops!"
  rectify_correct <- matrix(c("a", "b", "c", "d"),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, types = data_type), rectify_correct)
})

test_that("rectify() handles multiple data types", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L),
                  col = c(1L, 2L, 1L, 2L),
                  data_type = c("character", "character", "integer", "integer"),
                  character = c("a", "b", NA, NA),
                  integer = c(NA, NA, 3L, 4L),
                  stringsAsFactors = FALSE)
  rectify_correct <- matrix(c("a", "b", "3", "4"),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(1, 2),
                                            c("1(A)", "2(B)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x), rectify_correct)
})

test_that("Blank initial rows and columns are dropped", {
  x <- data.frame(row = c(2L, 2L, 3L, 3L),
                  col = c(2L, 3L, 2L, 3L),
                  data_type = rep("integer", 4),
                  integer = c(1L, 2L, 3L, NA),
                  stringsAsFactors = FALSE)
  rectify_correct <- matrix(as.character(c(1, 2, 3, NA)),
                            2L, 2L,
                            byrow = TRUE,
                            dimnames = list(c(2, 3),
                                            c("2(B)", "3(C)")))
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x), rectify_correct)
})

test_that("rectify() works on common data types", {
  x <- tibble::tibble(a = c(TRUE, FALSE),
                      b = c(1L, 2L),
                      c = c(1, 2),
                      d = c(1i, 2i),
                      e = c(Sys.Date(), Sys.Date() + 1),
                      f = c(Sys.time(), Sys.time() + 1),
                      g = c("a", "b"),
                      h = factor(c("c", "d")),
                      i = factor(c("e", "f"), ordered = TRUE),
                      j = list(1:2, letters[1:2]),
                      stringsAsFactors = FALSE)
  y <- tidy_table(x, colnames = TRUE)
  z <- rectify(y) # can't handle lists, because it converts everything to string.
})
