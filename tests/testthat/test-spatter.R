context("spatter")

x <- tidy_table(BOD, FALSE, TRUE)
y <-
  tidy_table(BOD, FALSE, TRUE) %>%
  behead(N, "header") %>%
  dplyr::select(-col, -chr)
z <-
  tidy_table(BOD, FALSE, TRUE) %>%
  behead(N, "header") %>%
  dplyr::mutate(data_type_2 = "chr") %>%
  dplyr::select(-col, -dbl)

test_that("spatter() works", {
  d <- spatter(y, header)
  expect_equal(colnames(d), c("row", "demand", "Time"))
  expect_equal(nrow(d), 6L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer", demand = "numeric", Time = "numeric"))
})

test_that("spatter() can use row and col as headers", {
  d <- spatter(x, row)
  expect_equal(colnames(d), c("col", as.character(1:7)))
  expect_equal(nrow(d), 2L)
  expect_equal(purrr::map_chr(d, class),
               `names<-`(c("integer", "character", rep("numeric", 6L)),
                         c("col", 1:7)))
  d <- spatter(x, col)
  expect_equal(colnames(d), c("row", "1", "2"))
  expect_equal(nrow(d), 7L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer", `1` = "character", `2` = "character"))
})

test_that("spatter() can use data_type as both types and header", {
  d <- spatter(x, data_type)
  expect_equal(colnames(d), c("row", "col", "chr", "dbl"))
  expect_equal(nrow(d), 14L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 col = "integer",
                 chr = "character",
                 dbl = "numeric"))
})

test_that("spatter() doesn't delete data_type if it isn't used", {
  d <- spatter(z, header, types = data_type_2)
  expect_equal(colnames(d), c("row", "data_type", "demand", "Time"))
  expect_equal(nrow(d), 6L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 data_type = "character",
                 demand = "logical",
                 Time = "logical"))
})

test_that("spatter() does delete the 'values' column", {
  d <- spatter(z, header, values = data_type_2)
  expect_equal(colnames(d), c("row", "chr", "demand", "Time"))
  expect_equal(nrow(d), 6L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 chr = "character",
                 demand = "character",
                 Time = "character"))
})

test_that("spatter() can use data_type as both types and values", {
  d <- spatter(dplyr::select(y, -dbl), header, values = data_type)
  expect_equal(colnames(d), c("row", "demand", "Time"))
  expect_equal(nrow(d), 6L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 demand = "character",
                 Time = "character"))
})

test_that("spatter() can use header as both header and values", {
  d <- spatter(dplyr::select(y, -dbl, -data_type), header, values = header)
  expect_equal(colnames(d), c("row", "demand", "Time"))
  expect_equal(nrow(d), 6L)
  expect_equal(d$demand, rep("demand", 6L))
  expect_equal(d$Time, rep("Time", 6L))
})

test_that("spatter() retains factors", {
  d <-
    dplyr::mutate(chickwts, feed = "a", feed = factor(feed)) %>%
    tibble::as_tibble() %>%
    tidy_table(FALSE, TRUE) %>%
    behead(N, header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 feed = "factor",
                 weight = "numeric"))
})

test_that("spatter() retains factors when some missing but no other types", {
  d <-
    dplyr::mutate(chickwts, feed = c("a", rep(NA, 70)), feed = factor(feed)) %>%
    tibble::as_tibble() %>%
    tidy_table(FALSE, TRUE) %>%
    behead(N, header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 feed = "factor",
                 weight = "numeric"))
})

test_that("spatter() converts factors to logical when all missing", {
  d <-
    dplyr::mutate(chickwts, feed = NA_character_, feed = factor(feed)) %>%
    tibble::as_tibble() %>%
    tidy_table(FALSE, TRUE) %>%
    behead(N, header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 feed = "logical",
                 weight = "numeric"))
})

test_that("spatter() converts ordered factors to character", {
  d <-
    dplyr::mutate(chickwts, feed = "a", feed = factor(feed, ordered = TRUE)) %>%
    tibble::as_tibble() %>%
    tidy_table(FALSE, TRUE) %>%
    behead(N, header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(purrr::map_chr(d, class),
               c(row = "integer",
                 feed = "factor",
                 weight = "numeric"))
})

test_that("spatter() can use custom functions for converting data types", {
  d <- spatter(x, row, chr = ~ paste0(., "-header"), dbl = as.integer)
  expect_equal(colnames(d), c("col", as.character(1:7)))
  expect_equal(nrow(d), 2L)
  expect_equal(purrr::map_chr(d, class),
               `names<-`(c("integer", "character", rep("integer", 6L)),
                         c("col", 1:7)))
  expect_equal(d$`1`, c("Time-header", "demand-header"))
})
