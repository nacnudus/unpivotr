context("test-pack.R")

w <- tibble::tibble(
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
x <- as_cells(w)

test_that("pack() works on common data types", {
  y <- pack(x)
  expect_equal(nrow(y), 20L)
  expect_equal(colnames(y), c("row", "col", "value"))
  expect_equal(
    unname(purrr::map_chr(y$value, pillar::type_sum)),
    c(
      rep(c(
        "lgl", "int", "dbl", "cpl", "date",
        "dttm", "chr", "fct", "ord"
      ),
      each = 2
      ),
      "int", "chr"
    )
  )
  expect_equal(
    names(y$value),
    rep(c(
      "lgl", "int", "dbl", "cpl", "date",
      "dttm", "chr", "fct", "ord", "list"
    ),
    each = 2
    )
  )
  expect_equal(y$value[[15]], factor("c", levels = c("c", "d")))
  expect_equal(y$value[[17]], factor("e", levels = c("e", "f"), ordered = TRUE))
})

test_that("unpack() works on common data types", {
  y <- pack(x)
  z <- unpack(y)
  expect(all(colnames(z) %in% colnames(x)), "Not all data types returned")
  expect(all(purrr::map_lgl(colnames(z), ~ identical(z[[.]], x[[.]]))),
         "Not all data returned")
})

test_that("unpack() orders type columns alphabetically", {
  y <- pack(x)
  z <- unpack(y)
  expect_equal(
    colnames(z),
    c(
      "row", "col", "data_type", "chr", "cpl", "date",
      "dbl", "dttm", "fct", "int", "lgl", "list", "ord"
    )
  )
})
