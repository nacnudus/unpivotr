context("test-concatenate.R")

test_that("concatenate() works on common datatypes", {
  expect_equal(
    concatenate(list(NULL, NA, TRUE)),
    c(NA, NA, TRUE)
  )
  expect_equal(
    concatenate(list(NULL, NA_integer_, 1L)),
    c(NA_integer_, NA_integer_, 1L)
  )
  expect_equal(
    concatenate(list(NULL, NA_real_, 1)),
    c(NA_real_, NA_real_, 1L)
  )
  expect_equal(
    concatenate(list(NULL, NA_complex_, 1i)),
    c(NA_complex_, NA_complex_, 1i)
  )
  expect_equal(
    concatenate(list(NULL, NA_complex_, 1i)),
    c(NA_complex_, NA_complex_, 1i)
  )
  expect_equal(
    concatenate(list(NULL, as.Date(NA), as.Date("2001-01-01"))),
    as.Date(c(NA, NA, "2001-01-01"))
  )
  expect_equal(
    concatenate(list(
      NULL,
      as.POSIXct(NA),
      as.POSIXct("2001-01-01 01:01:01")
    )),
    as.POSIXct(c(NA, NA, "2001-01-01 01:01:01"))
  )
  expect_equal(
    concatenate(list(NULL, NA_character_, "a")),
    c(NA_character_, NA_character_, "a")
  )
  expect_equal(
    concatenate(list(NULL, factor(NA), factor("a"), factor("b"))),
    factor(c(NA, NA, "a", "b"))
  )
  expect_equal(
    concatenate(list(
      NULL,
      factor(NA, ordered = TRUE),
      factor("a", ordered = TRUE)
    )),
    factor(c(NA, NA, "a"))
  ) # ordered factors are coerced to factors
  expect_equal(
    concatenate(list(NULL, NA, 1:2)),
    list(NULL, NA, 1:2)
  ) # lists are returned as-is
})

test_that("concatenate() factor features work", {
  x <- list(NULL, factor(NA), factor("a"), factor("b"))
  expect_equal(
    concatenate(x),
    factor(c(NA, NA, "a", "b"))
  )
  expect_equal(
    concatenate(x, fill_factor_na = FALSE),
    factor(c(NA, NA, "a", "b"))
  )
  expect_equal(
    concatenate(x, combine_factors = FALSE),
    list(
      factor(NA), factor(NA),
      factor("a"), factor("b")
    )
  )
  expect_equal(
    concatenate(x, combine_factors = FALSE, fill_factor_na = FALSE),
    list(NULL, factor(NA), factor("a"), factor("b"))
  )
  x <- list(NULL, factor(NA), NULL, factor(NA))
  expect_equal(
    concatenate(x, combine_factors = FALSE, fill_factor_na = FALSE),
    list(NULL, factor(NA), NULL, factor(NA))
  )
})

test_that("concatenate() handles all-null lists", {
  x <- list(NULL, NULL)
  expect_equal(concatenate(x), x)
})

test_that("concatenate() handles timezones", {
  x <- as.POSIXct("2018-01-01 01:00:00", tz = "UTC")
  y <- as.POSIXct("2018-04-01 01:00:00", tz = "UTC")
  expect_equal(
    concatenate(list(x, y)),
    as.POSIXct(c(
      "2018-01-01 01:00:00",
      "2018-04-01 01:00:00"
    ),
    tz = "UTC"
    )
  )
  x <- as.POSIXct("2018-01-01 01:00:00", tz = "UTC")
  y <- as.POSIXct("2018-04-01 01:00:00", tz = "EST")
  expect_equal(
    concatenate(list(x, y)),
    as.POSIXct(c(
      "2018-01-01 01:00:00",
      "2018-04-01 06:00:00"
    ),
    tz = "UTC"
    )
  )
})
