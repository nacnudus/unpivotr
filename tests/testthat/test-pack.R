context("test-pack.R")

w <- tibble::tibble(
  lgl = c(TRUE, FALSE),
  int = c(1L, 2L),
  dbl = c(1, 2),
  cplx = c(1i, 2i),
  date = c(as.Date("2001-01-01"), as.Date("2001-01-02")),
  dttm = c(
    as.POSIXct("2001-01-01 01:01:01"),
    as.POSIXct("2001-01-01 01:01:02")
  ),
  chr = c("a", "b"),
  fctr = factor(c("c", "d")),
  ord = factor(c("e", "f"), ordered = TRUE),
  list = list(1:2, letters[1:2])
)
x <- as_cells(w)

test_that("pack() works on common data types", {
  y <- pack(x)
  expect_equal(nrow(y), 20L)
  expect_equal(colnames(y), c("row", "col", "value"))
  expect_equal(
    purrr::map_chr(y$value[-19:-20], vctrs::vec_ptype_abbr),
    rep(purrr::map_chr(w[, -10], vctrs::vec_ptype_abbr), each = 2)
  )
  expect_equal(
    names(y$value),
    unname(rep(purrr::map_chr(w, vctrs::vec_ptype_abbr), each = 2))
  )
  expect_equal(y$value[[15]], factor("c", levels = c("c", "d")))
  expect_equal(y$value[[17]], factor("e", levels = c("e", "f"), ordered = TRUE))
})

test_that("unpack() works on common data types", {
  y <- pack(x)
  z <- unpack(y)
  expect(all(colnames(z) %in% colnames(x)),
         "Not all data types worked")
  expect(all(purrr::map_lgl(colnames(z), ~ identical(z[[.]], x[[.]]))),
         "Not all data types worked")
})

test_that("unpack() orders type columns alphabetically", {
  y <- pack(x)
  z <- unpack(y)
  expect_equal(colnames(x), colnames(z))
})
