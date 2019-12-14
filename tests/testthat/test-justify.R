context("justify")

test_that("justify() works", {
  x <- tibble::tibble(row = c(2L, 4L), col = c(2L, 4L), value = c("a", "b"))
  y <- tibble::tibble(row = c(1L, 3L), col = c(1L, 3L))
  z <- tibble::tibble(row = c(1L, 3L), col = c(1L, 3L), value = c("a", "b"))
  expect_equal(justify(x, y), z)
  expect_error(justify(x, y[-1, ]),
    "nrow(header_cells) == nrow(corner_cells) is not TRUE",
    fixed = TRUE
  )
  expect_error(justify(x[-1, ], y),
    "nrow(header_cells) == nrow(corner_cells) is not TRUE",
    fixed = TRUE
  )
})
