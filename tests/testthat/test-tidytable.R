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
    list(tibble::tribble(~row, ~col,                                    ~html,
                           1L,   1L, "<th rowspan=\"2\">Header (1:2, 1)</th>",
                           2L,   1L,                                       NA,
                           1L,   2L,                 "<th>Header (1, 2)</th>",
                           2L,   2L,                   "<td>cell (2, 2)</td>"))
  colspan_correct <-
    list(tibble::tribble(~row, ~col,                                    ~html,
                           1L,   1L, "<th colspan=\"2\">Header (1, 1:2)</th>",
                           2L,   1L,                   "<td>cell (2, 1)</td>",
                           1L,   2L,                                       NA,
                           2L,   2L,                   "<td>cell (2, 2)</td>"))
  rowandcolspan_correct <-
    list(tibble::tribble(~row, ~col,                                                    ~html,
                           1L,   1L, "<th colspan=\"2\" rowspan=\"2\">Header (1:2, 1:2)</th>",
                           2L,   1L,                                                       NA,
                           1L,   2L,                                                       NA,
                           2L,   2L,                                                       NA,
                           1L,   3L,                                 "<th>Header (2, 3)</th>",
                           2L,   3L,                                   "<td>cell (3, 1)</td>",
                           1L,   4L,                                                       NA,
                           2L,   4L,                                   "<td>cell (3, 2)</td>",
                           1L,   5L,                                                       NA,
                           2L,   5L,                                   "<td>cell (3, 3)</td>"))
  nested_correct <-
    list(tibble::tribble(~row,  ~col,                          ~html,
                           1L,    1L, "<th>Header (2, 2)(1, 1)</th>",
                           2L,    1L,   "<td>cell (2, 2)(2, 1)</td>",
                           1L,    2L, "<th>Header (2, 2)(1, 2)</th>",
                           2L,    2L,   "<td>cell (2, 2)(2, 1)</td>"))
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
