context("tidy_table()")

library(dplyr)
library(rvest)

test_that("tidy_table works with or without row and column names", {
  expect_equal(nrow(tidy_table(Formaldehyde)), 12)
  expect_equal(nrow(tidy_table(Formaldehyde, colnames = TRUE)), 14)
  expect_equal(nrow(tidy_table(Formaldehyde, rownames = TRUE)), 18)
  expect_equal(nrow(tidy_table(Formaldehyde, rownames = TRUE, colnames = TRUE)), 20)
})

test_that("tidy_table works with html tables", {
  rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
  colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
  rowandcolspan <- system.file("extdata", "row-and-colspan.html", package = "unpivotr")
  nested <- system.file("extdata", "nested.html", package = "unpivotr")
  rowspan_correct <-
    list(tibble::tribble(~row, ~col, ~data_type,                                    ~html,
                           1L,   1L,     "html", "<th rowspan=\"2\">Header (1:2, 1)</th>",
                           2L,   1L,     "html",                                       NA,
                           1L,   2L,     "html",                 "<th>Header (1, 2)</th>",
                           2L,   2L,     "html",                   "<td>cell (2, 2)</td>"))
  colspan_correct <-
    list(tibble::tribble(~row, ~col, ~data_type,                                    ~html,
                           1L,   1L,     "html", "<th colspan=\"2\">Header (1, 1:2)</th>",
                           2L,   1L,     "html",                   "<td>cell (2, 1)</td>",
                           1L,   2L,     "html",                                       NA,
                           2L,   2L,     "html",                   "<td>cell (2, 2)</td>"))
  rowandcolspan_correct <-
    list(tibble::tribble(~row, ~col, ~data_type,                                                     ~html,
                           1L,   1L,     "html", "<th colspan=\"2\" rowspan=\"2\">Header (1:2, 1:2)</th>",
                           2L,   1L,     "html",                                                       NA,
                           1L,   2L,     "html",                                                       NA,
                           2L,   2L,     "html",                                                       NA,
                           1L,   3L,     "html",                                 "<th>Header (2, 3)</th>",
                           2L,   3L,     "html",                                   "<td>cell (3, 1)</td>",
                           1L,   4L,     "html",                                                       NA,
                           2L,   4L,     "html",                                   "<td>cell (3, 2)</td>",
                           1L,   5L,     "html",                                                       NA,
                           2L,   5L,     "html",                                   "<td>cell (3, 3)</td>"))
  nested_correct <-
    list(tibble::tribble(~row,  ~col, ~data_type,                          ~html,
                           1L,    1L,     "html", "<th>Header (2, 2)(1, 1)</th>",
                           2L,    1L,     "html",   "<td>cell (2, 2)(2, 1)</td>",
                           1L,    2L,     "html", "<th>Header (2, 2)(1, 2)</th>",
                           2L,    2L,     "html",   "<td>cell (2, 2)(2, 1)</td>"))
  rowspan_parsed <-
    rowspan %>%
    read_html() %>%
    tidy_table()
  colspan_parsed <-
    colspan %>%
    read_html() %>%
    tidy_table()
  rowandcolspan_parsed <-
    rowandcolspan %>%
    read_html() %>%
    tidy_table()
  nested_parsed <-
    nested %>%
    read_html() %>%
    tidy_table() %>%
    .[[1]] %>%
    .$html %>%
    .[4] %>%
    read_html() %>%
    tidy_table()
  expect_identical(rowspan_parsed, rowspan_correct)
  expect_identical(colspan_parsed, colspan_correct)
  expect_identical(rowandcolspan_parsed, rowandcolspan_correct)
  expect_identical(nested_parsed, nested_correct)
})

test_that("tidy_table works with all common datatypes", {
  x <- data.frame(lgl = c(TRUE, FALSE),
                  int = c(1L, 2L),
                  dbl = c(1, 2),
                  cpl = c(1i, 2i),
                  raw = as.raw(c(11, 12)),
                  date = c(Sys.Date(), Sys.Date() + 1),
                  dttm = c(Sys.time(), Sys.time() + 1),
                  chr = c("a", "b"),
                  fct = factor(c("c", "d")),
                  stringsAsFactors = FALSE)
  y <- tidy_table(x)
  expect_equal(colnames(y),
               c("row", "col", "data_type", "chr", "cpl", "date", "dbl", "dttm",
                 "fct", "int", "lgl", "raw"))
  x_class <- purrr::map(x, class)
  y_class <- purrr::map(y, class)
  expect_equal(y_class[names(x_class)], x_class)
})
