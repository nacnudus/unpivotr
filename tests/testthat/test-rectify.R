context("rectify")

test_that("rectify() has the right row/col headers", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L),
    col = c(1L, 2L, 1L, 2L),
    data_type = rep("chr", 4),
    chr = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = 1:2,
    `2(B)` = 1:2,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, values = row), rectify_correct)
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = 1L,
    `2(B)` = 2L,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, values = col), rectify_correct)
})

test_that("rectify() isn't confused by same-named variables in context", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L),
    col = c(1L, 2L, 1L, 2L),
    data_type = rep("chr", 4),
    chr = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  data_type <- "Oops!"
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = c("a", "c"),
    `2(B)` = c("b", "d"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, types = data_type), rectify_correct)
})

test_that("rectify() can use 'row', 'col' and 'data_type' as values", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L),
    col = c(1L, 2L, 1L, 2L),
    data_type = c("chr", "chr", "int", "int"),
    chr = c("a", "b", NA, NA),
    int = c(NA, NA, 3L, 4L),
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = 1:2,
    `2(B)` = 1:2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, values = row), rectify_correct)
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = 1L,
    `2(B)` = 2L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, values = col), rectify_correct)
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = c("chr", "int"),
    `2(B)` = c("chr", "int"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x, values = data_type), rectify_correct)
})

test_that("Blank initial rows and columns are handled", {
  x <- data.frame(
    row = c(2L, 2L, 3L, 3L),
    col = c(2L, 3L, 2L, 3L),
    data_type = rep("int", 4),
    int = c(1L, 2L, 3L, NA),
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 2:3,
    `2(B)` = c(1L, 3L),
    `3(C)` = c(2L, NA_integer_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x), rectify_correct)
})

test_that("rectify() allows formatting formulas", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L),
    col = c(1L, 2L, 1L, 2L),
    data_type = c("chr", "chr", "int", "int"),
    chr = c("a", "b", NA, NA),
    int = c(NA, NA, 3L, 4L),
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 1:2,
    `1(A)` = c("A", "30"),
    `2(B)` = c("B", "40"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(
    rectify(x, formatters = list(chr = toupper, int = ~ .x * 10)),
    rectify_correct
  )
})

test_that("Extraneous columns on the edges are dropped", {
  x <- data.frame(
    row = c(2L, 2L, 3L, 3L),
    col = c(2L, 3L, 2L, 3L),
    data_type = rep("int", 4),
    int = c(1L, 2L, 3L, NA),
    foo = 1:4,
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 2:3,
    `2(B)` = c(1L, 3L),
    `3(C)` = c(2L, NA_integer_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x), rectify_correct)
})

test_that("Blank columns amongst the data are retained", {
  x <- data.frame(
    row = c(2L, 2L, 4L, 4L),
    col = c(2L, 4L, 2L, 4L),
    data_type = rep("int", 4),
    int = c(1L, 2L, 3L, NA),
    stringsAsFactors = FALSE
  )
  rectify_correct <- data.frame(
    `row/col` = 2:4,
    `2(B)` = c(1L, NA_integer_, 3L),
    `3(C)` = c(NA, NA, NA),
    `4(D)` = c(2L, NA_integer_, NA_integer_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  class(rectify_correct) <- c("cell_grid", class(rectify_correct))
  expect_equal(rectify(x), rectify_correct)
})

test_that("rectify() handles zero-row data frames", {
  expect_error(
    rectify(dplyr::slice(as_cells(mtcars, col_names = TRUE), 0L)),
    NA
  )
})

test_that("rectify() stops on non-distinct cells", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L),
    col = c(1L, 2L, 1L, 2L),
    data_type = rep("chr", 4),
    chr = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  expect_error(
    rectify(dplyr::bind_rows(x, x), values = row),
    "Row and column numbers must be distinct.\n  Perhaps you meant to use a single sheet.",
    fixed = TRUE
  )
})

test_that("rectify() works on common data types", {
  x <-
    tibble::tibble(
      lgl = c(TRUE, FALSE),
      int = c(1L, 2L),
      dbl = c(1, 2),
      cpl = c(1i, 2i),
      date = c(as.Date("2001-01-01"), as.Date("2001-01-02")),
      chr = c("a", "b"),
      fct = factor(c("c", "d")),
      list = list(1:2, letters[1:2])
    )
  y <-
    as_cells(x) %>%
    rectify() %>%
    dplyr::select(-1)
  expect(all(purrr::map2_lgl(y, x, identical)), "Not all data returned")
})
