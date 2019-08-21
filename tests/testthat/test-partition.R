context("partition")

test_that("partition_dim() works", {
  # Positions before the first partition are put into group 0
  expect_equal(
    partition_dim(1:10, c(3, 5, 7)),
    c(NA, NA, 3, 3, 5, 5, 7, 7, 7, 7)
  )
  # Groups are numbered in reverse for lower bounds
  expect_equal(
    partition_dim(1:10, c(3, 5, 7), bound = "lower"),
    c(3, 3, 3, 5, 5, 7, 7, NA, NA, NA)
  )
  # When the first row of cells is on the first cutpoint, there is no group 0.
  expect_equal(
    partition_dim(1:10, c(1, 10)),
    c(rep(1, 9), 10)
  )
  expect_equal(
    partition_dim(1:10, c(1, 10), bound = "lower"),
    c(1, rep(10, 9))
  )
  # Non-integer row/column numbers and cutpoints can be used, even though they
  # make no sense in the context of partioning grids of cells.
  expect_equal(
    partition_dim(1:10 - .5, c(3, 5, 7)),
    c(NA, NA, NA, 3, 3, 5, 5, 7, 7, 7)
  )
  expect_equal(
    partition_dim(1:10, c(3, 5, 7) + 1.5),
    c(NA, NA, NA, NA, 4, 4, 6, 6, 8, 8)
  )
  # When there is no cutpoint, everything is put into group 0
  expect_equal(
    partition_dim(1:10, integer()),
    rep(NA_integer_, 10)
  )
})

test_that("partition_dim() catches argument errors", {
  expect_error(partition_dim(1:10))
  expect_error(partition_dim(1:10, 5, "left"))
})

test_that("partition() works", {
  multiples <- dplyr::arrange(purpose$small_multiples, col, row)
  tl_corners <-
    dplyr::filter(
      multiples,
      !is.na(character),
      !(character %in% c("Sex", "Value", "Female", "Male"))
    ) %>%
    dplyr::select(row, col, corner = character)
  expect_equal(
    partition(multiples, tl_corners, nest = FALSE)$corner_row,
    rep(c(1, 6, 11, 1, 6), each = 8)
  )
  bl_corners <-
    dplyr::filter(multiples, character == "Male") %>%
    dplyr::select(row, col, corner = character)
  expect_equal(
    partition(multiples,
      bl_corners,
      align = "bottom_left",
      nest = FALSE
    )$corner_row,
    rep(c(4, 9, 14, 4, 9), each = 8)
  )
  tr_corners <-
    multiples %>%
    dplyr::filter(character == "Value") %>%
    dplyr::mutate(row = row - 1) %>%
    dplyr::select(row, col, corner = character)
  expect_equal(
    partition(multiples,
      tr_corners,
      align = "top_right",
      nest = FALSE
    )$corner_row,
    rep(c(1, 6, 11, 1, 6), each = 8)
  )
  br_corners <-
    multiples %>%
    dplyr::filter(character == "Male") %>%
    dplyr::mutate(col = col + 1) %>%
    dplyr::select(row, col, corner = character)
  expect_equal(
    partition(multiples,
      br_corners,
      align = "bottom_right",
      nest = FALSE
    )$corner_row,
    rep(c(4, 9, 14, 4, 9), each = 8)
  )
})

test_that("partition() arguments are checked", {
  multiples <- purpose$small_multiples
  tl_corners <-
    dplyr::filter(
      multiples,
      !is.na(character),
      !(character %in% c("Sex", "Value", "Female", "Male"))
    )
  expect_error(partition(multiples, tl_corners, "top_bottom"))
})

test_that("partition() stops on non-distinct cells", {
  multiples <- dplyr::arrange(purpose$small_multiples, col, row)
  tl_corners <-
    dplyr::filter(
      multiples,
      !is.na(character),
      !(character %in% c("Sex", "Value", "Female", "Male"))
    )
  expect_error(
    partition(dplyr::bind_rows(multiples, multiples), tl_corners, nest = FALSE),
    "Row and column numbers must be distinct.\n  Perhaps you meant to use a single sheet.",
    fixed = TRUE
  )
})
