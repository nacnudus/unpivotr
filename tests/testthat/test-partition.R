context("partition")

test_that("partition_dim() works", {
  # Positions before the first partition are put into group 0
  expect_equal(partition_dim(1:10, c(3, 5, 7)),
               c(0, 0, 1, 1, 2, 2, 3, 3, 3, 3))
  # Groups are numbered in reverse for lower bounds
  expect_equal(partition_dim(1:10, c(3, 5, 7), bound = "lower"),
               c(3, 3, 3, 2, 2, 1, 1, 0, 0, 0))
  # When the first row of cells is on the first cutpoint, there is no group 0.
  expect_equal(partition_dim(1:10, c(1, 10)),
               c(rep(1, 9), 2))
  expect_equal(partition_dim(1:10, c(1, 10), bound = "lower"),
               c(2, rep(1, 9)))
  # Non-integer row/column numbers and cutpoints can be used, even though they
  # make no sense in the context of partioning grids of cells.
  expect_equal(partition_dim(1:10 - .5, c(3, 5, 7)),
               c(0, 0, 0, 1, 1, 2, 2, 3, 3, 3))
  expect_equal(partition_dim(1:10, c(3, 5, 7) + 1.5),
               c(0, 0, 0, 0, 1, 1, 2, 2, 3, 3))
  # When there is no cutpoint, everything is put into group 0
  expect_equal(partition_dim(1:10, integer()),
               rep(0, 10))
})

test_that("partition_dim() catches argument errors", {
  expect_error(partition_dim(1:10))
  expect_error(partition_dim(1:10, 5, "left"))
})

test_that("partition() works", {
  multiples <- dplyr::arrange(purpose$small_multiples, col, row)
  tl_corners <-
    dplyr::filter(multiples,
                  !is.na(character),
                  !(character %in% c("Sex", "Value", "Female", "Male")))
  expect_equal(partition(multiples, tl_corners, nest = FALSE)$partition,
               rep(c(1, 3, 5, 2, 4), each = 8))
  bl_corners <- dplyr::filter(multiples, character == "Male")
  expect_equal(partition(multiples,
                         bl_corners,
                         align = "bottom_left",
                         nest = FALSE)$partition,
               rep(c(4, 2, 1, 5, 3), each = 8))
  tr_corners <-
    multiples %>%
    dplyr::filter(character == "Value") %>%
    dplyr::mutate(row = row - 1)
  expect_equal(partition(multiples,
                         tr_corners,
                         align = "top_right",
                         nest = FALSE)$partition,
               rep(c(2, 4, 5, 1, 3), each = 8))
  br_corners <-
    multiples %>%
    dplyr::filter(character == "Male") %>%
    dplyr::mutate(col = col + 1)
  expect_equal(partition(multiples,
                         br_corners,
                         align = "bottom_right",
                         nest = FALSE)$partition,
               rep(c(5, 3, 1, 4, 2), each = 8))
})

test_that("partition() arguments are checked", {
  multiples <- purpose$small_multiples
  tl_corners <-
    dplyr::filter(multiples,
                  !is.na(character),
                  !(character %in% c("Sex", "Value", "Female", "Male")))
  expect_error(partition(multiples, tl_corners, "top_bottom"))
})
