context("utils")

test_that("direction dialects are converted", {
  compass <- c("NNW", "N", "NNE",
               "ENE", "E", "ESE",
               "SSE", "S", "SSW",
               "WSW", "W", "WNW",
               "ABOVE", "RIGHT", "BELOW", "LEFT")
  standard <- c("up-left", "up", "up-right",
                "right-up", "right", "right-down",
                "down-right", "down", "down-left",
                "left-down", "left", "left-up",
                "up-ish", "right-ish", "down-ish", "left-ish")
  expect_identical(
    vapply(standard, standardise_direction, character(1), USE.NAMES = FALSE),
    standard
  )
  expect_identical(
    vapply(compass, standardise_direction, character(1), USE.NAMES = FALSE),
    standard
  )
})

test_that("incorrect directions are reported", {
  expect_error(standardise_direction("foo"),
               "The direction \"foo\" is not recognised.  See \\?directions.")
})

test_that("only scalar directions are accepted", {
  expect_error(standardise_direction(LETTERS[1:2]))
})
