context("spatter")

# upper/lowercase sort order depends on the locale
colnames(BOD) <- c("time", "demand")

x <- as_cells(BOD, FALSE, TRUE)
y <-
  as_cells(BOD, FALSE, TRUE) %>%
  behead("N", "header") %>%
  dplyr::select(-col, -chr)
z <-
  as_cells(BOD, FALSE, TRUE) %>%
  behead("N", "header") %>%
  dplyr::mutate(data_type_2 = "chr") %>%
  dplyr::select(-col, -dbl)

test_that("spatter() works", {
  d <- spatter(y, header)
  expect_equal(colnames(d), c("row", "demand", "time"))
  expect_equal(nrow(d), 6L)
  expect_equal(
    purrr::map_chr(d, class),
    c(row = "integer", demand = "numeric", time = "numeric")
  )
})

test_that("spatter() can use row and col as headers", {
  d <- spatter(x, row)
  expect_equal(colnames(d), c("col", as.character(1:7)))
  expect_equal(nrow(d), 2L)
  expect_equal(
    purrr::map_chr(d, class),
    `names<-`(
      c("integer", "character", rep("numeric", 6L)),
      c("col", 1:7)
    )
  )
  d <- spatter(x, col)
  expect_equal(colnames(d), c("row", "1", "2"))
  expect_equal(nrow(d), 7L)
  expect_equal(
    purrr::map_chr(d, class),
    c(row = "integer", `1` = "character", `2` = "character")
  )
})

test_that("spatter() can use data_type as both types and header", {
  d <- spatter(x, data_type)
  expect_equal(colnames(d), c("row", "col", "chr", "dbl"))
  expect_equal(nrow(d), 14L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      col = "integer",
      chr = "character",
      dbl = "numeric"
    )
  )
})

test_that("spatter() doesn't delete data_type if it isn't used", {
  d <- spatter(z, header, types = data_type_2)
  expect_equal(colnames(d), c("row", "data_type", "demand", "time"))
  expect_equal(nrow(d), 6L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      data_type = "character",
      demand = "character",
      time = "character"
    )
  )
})

test_that("spatter() does delete the 'values' column", {
  d <- spatter(z, header, values = data_type_2)
  expect_equal(colnames(d), c("row", "chr", "demand", "time"))
  expect_equal(nrow(d), 6L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      chr = "character",
      demand = "character",
      time = "character"
    )
  )
})

test_that("spatter() can use data_type as both types and values", {
  d <- spatter(dplyr::select(y, -dbl), header, values = data_type)
  expect_equal(colnames(d), c("row", "demand", "time"))
  expect_equal(nrow(d), 6L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      demand = "character",
      time = "character"
    )
  )
})

test_that("spatter() can use header as both header and values", {
  d <- spatter(dplyr::select(y, -dbl, -data_type), header, values = header)
  expect_equal(colnames(d), c("row", "demand", "time"))
  expect_equal(nrow(d), 6L)
  expect_equal(d$demand, rep("demand", 6L))
  expect_equal(d$time, rep("time", 6L))
})

test_that("spatter() retains factors", {
  d <-
    dplyr::mutate(chickwts, feed = "a", feed = factor(feed)) %>%
    tibble::as_tibble() %>%
    as_cells(FALSE, TRUE) %>%
    behead("N", header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      feed = "factor",
      weight = "numeric"
    )
  )
})

test_that("spatter() retains factors when some missing but no other types", {
  d <-
    dplyr::mutate(chickwts, feed = c("a", rep(NA, 70)), feed = factor(feed)) %>%
    tibble::as_tibble() %>%
    as_cells(FALSE, TRUE) %>%
    behead("N", header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      feed = "factor",
      weight = "numeric"
    )
  )
})

test_that("spatter() doesn't convert factors to logical when all missing", {
  d <-
    tibble::tibble(
      dbl = c(1, 2),
      fct = factor(NA, NA),
      ord = factor(NA, NA, ordered = TRUE)
    ) %>%
    as_cells(col_names = TRUE) %>%
    behead("N", header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "dbl", "fct", "ord"))
  expect_equal(nrow(d), 2L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      dbl = "numeric",
      fct = "factor",
      ord = "factor"
    )
  )
})

test_that("spatter() converts ordered factors to character", {
  d <-
    dplyr::mutate(chickwts, feed = "a", feed = factor(feed, ordered = TRUE)) %>%
    tibble::as_tibble() %>%
    as_cells(FALSE, TRUE) %>%
    behead("N", header) %>%
    dplyr::select(-col, -chr) %>%
    spatter(header)
  expect_equal(colnames(d), c("row", "feed", "weight"))
  expect_equal(nrow(d), 71L)
  expect_equal(
    purrr::map_chr(d, class),
    c(
      row = "integer",
      feed = "factor",
      weight = "numeric"
    )
  )
})

test_that("spatter() can use custom functions for converting data types", {
  d <- spatter(x, row,
    formatters = list(
      chr = ~ paste0(., "-header"),
      dbl = as.integer
    )
  )
  expect_equal(colnames(d), c("col", as.character(1:7)))
  expect_equal(nrow(d), 2L)
  expect_equal(
    purrr::map_chr(d, class),
    `names<-`(
      c("integer", "character", rep("integer", 6L)),
      c("col", 1:7)
    )
  )
  expect_equal(d$`1`, c("time-header", "demand-header"))
})

test_that("spatter() works on common data types", {
  x <-
    tibble::tibble(
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
      list = list(1:2, letters[1:2])
    )
  y <-
    as_cells(x, col_names = TRUE) %>%
    behead("N", header) %>%
    dplyr::select(-col) %>%
    spatter(header) %>%
    dplyr::select(-row)
  expect_equal(colnames(y), sort(colnames(x)))
  x_class <- purrr::map(x, class)
  y_class <- purrr::map(y, class)
  expect_equal(y_class[names(x_class)], x_class)
})
