context("test-isolate_sentinels.R")

x <- tibble::tibble(
  lgl = c(TRUE, FALSE),
  int = c(1L, 2L),
  dbl = c(1, 2),
  cpl = c(1i, 2i),
  date = c(as.Date("2001-01-01"), as.Date("2001-01-02")),
  dttm = c(
    as.POSIXct("2001-01-01 01:01:01"),
    as.POSIXct("2001-01-01 01:01:02")
  ),
  chr = c("a", "b"),
  fct = factor(c("c", "d")),
  ord = factor(c("e", "f"), ordered = TRUE),
  list = list(1:2, letters[1:2])
)

test_that("isolate_sentinels() works on common data types", {
  y <- isolate_sentinels(x, lgl, TRUE)
  expect_equal(y$lgl, c(NA, FALSE))
  expect_equal(y$sentinel, c(TRUE, NA))
  y <- isolate_sentinels(x, int, 1L)
  expect_equal(y$int, c(NA_integer_, 2L))
  expect_equal(y$sentinel, c(1L, NA_integer_))
  y <- isolate_sentinels(x, dbl, 1)
  expect_equal(y$dbl, c(NA_real_, 2))
  expect_equal(y$sentinel, c(1L, NA_real_))
  y <- isolate_sentinels(x, cpl, 1i)
  expect_equal(y$cpl, c(NA_complex_, 2i))
  expect_equal(y$sentinel, c(1i, NA_complex_))
  y <- isolate_sentinels(x, date, as.Date("2001-01-01"))
  expect_equal(y$date, as.Date(c(NA_real_, "2001-01-02")))
  expect_equal(y$sentinel, as.Date(c("2001-01-01", NA_real_)))
  y <- isolate_sentinels(x, dttm, as.POSIXct("2001-01-01 01:01:01"))
  expect_equal(y$dttm, as.POSIXct(c(NA_real_, "2001-01-01 01:01:02")))
  expect_equal(y$sentinel, as.POSIXct(c("2001-01-01 01:01:01", NA_real_)))
  y <- isolate_sentinels(x, chr, "a")
  expect_equal(y$chr, c(NA_character_, "b"))
  expect_equal(y$sentinel, c("a", NA_character_))
  expect_error(isolate_sentinels(x, fct, NA))
  expect_error(isolate_sentinels(x, ord, NA))
  expect_error(isolate_sentinels(x, list, c("e", "f")))
})

test_that("isolate_sentinels() is vectorised over sentinel values", {
  y <- isolate_sentinels(x, chr, c("a", "b"))
  expect_equal(y$chr, c(NA_character_, NA_character_))
  expect_equal(y$sentinel, c("a", "b"))
})

test_that("isolate_sentinels() works when the sentinel doesn't appear", {
  y <- isolate_sentinels(x, chr, "c")
  expect_equal(y$chr, c("a", "b"))
  expect_equal(y$sentinel, c(NA_character_, NA_character_))
})

test_that("isolate_sentinels() allows a custom name for the new column", {
  y <- isolate_sentinels(x, chr, "a", "foo")
  expect_equal(y$foo, c("a", NA_character_))
})

test_that("isolate_sentinels() requires type agreement", {
  expect_error(isolate_sentinels(x, chr, 1L))
})
