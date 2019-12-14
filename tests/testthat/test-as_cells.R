context("as_cells()")

test_that("as_cells() works with or without row and column names", {
  expect_equal(nrow(as_cells(Formaldehyde)), 12)
  expect_equal(nrow(as_cells(Formaldehyde, col_names = TRUE)), 14)
  expect_equal(nrow(as_cells(Formaldehyde, row_names = TRUE)), 18)
  expect_equal(nrow(as_cells(Formaldehyde, row_names = TRUE, col_names = TRUE)), 20)
})

test_that("as_cells() works with html tables", {
  rowspan <- system.file("extdata", "rowspan.html", package = "unpivotr")
  colspan <- system.file("extdata", "colspan.html", package = "unpivotr")
  rowandcolspan <- system.file("extdata", "row-and-colspan.html", package = "unpivotr")
  nested <- system.file("extdata", "nested.html", package = "unpivotr")
  rowspan_correct <-
    list(tibble::tribble(
      ~ row, ~ col, ~ data_type,                                   ~ html,
         1L,    1L,      "html", "<th rowspan=\"2\">Header (1:2, 1)</th>",
         2L,    1L,      "html",                                       NA,
         1L,    2L,      "html",                 "<th>Header (1, 2)</th>",
         2L,    2L,      "html",                   "<td>cell (2, 2)</td>")
    )
  colspan_correct <-
    list(tibble::tribble(
      ~ row, ~ col, ~ data_type,                                   ~ html,
         1L,    1L,      "html", "<th colspan=\"2\">Header (1, 1:2)</th>",
         2L,    1L,      "html",                   "<td>cell (2, 1)</td>",
         1L,    2L,      "html",                                       NA,
         2L,    2L,      "html",                   "<td>cell (2, 2)</td>")
    )
  rowandcolspan_correct <-
    list(tibble::tribble(
      ~ row, ~ col, ~ data_type,                                                    ~ html,
         1L,    1L,      "html", "<th colspan=\"2\" rowspan=\"2\">Header (1:2, 1:2)</th>",
         2L,    1L,      "html",                                                       NA,
         1L,    2L,      "html",                                                       NA,
         2L,    2L,      "html",                                                       NA,
         1L,    3L,      "html",                                 "<th>Header (2, 3)</th>",
         2L,    3L,      "html",                                   "<td>cell (3, 1)</td>",
         1L,    4L,      "html",                                                       NA,
         2L,    4L,      "html",                                   "<td>cell (3, 2)</td>",
         1L,    5L,      "html",                                                       NA,
         2L,    5L,      "html",                                   "<td>cell (3, 3)</td>")
    )
  nested_correct <-
    list(tibble::tribble(
      ~ row,  ~ col, ~ data_type,                       ~ html,
         1L,    1L,     "html", "<th>Header (2, 2)(1, 1)</th>",
         2L,    1L,     "html",   "<td>cell (2, 2)(2, 1)</td>",
         1L,    2L,     "html", "<th>Header (2, 2)(1, 2)</th>",
         2L,    2L,     "html",   "<td>cell (2, 2)(2, 1)</td>")
    )
  rowspan_parsed <-
    rowspan %>%
    xml2::read_html() %>%
    as_cells()
  colspan_parsed <-
    colspan %>%
    xml2::read_html() %>%
    as_cells()
  rowandcolspan_parsed <-
    rowandcolspan %>%
    xml2::read_html() %>%
    as_cells()
  nested_parsed <-
    nested %>%
    xml2::read_html() %>%
    as_cells() %>%
    .[[1]] %>%
    .$html %>%
    .[4] %>%
    xml2::read_html() %>%
    as_cells()
  expect_identical(rowspan_parsed, rowspan_correct)
  expect_identical(colspan_parsed, colspan_correct)
  expect_identical(rowandcolspan_parsed, rowandcolspan_correct)
  expect_identical(nested_parsed, nested_correct)
})

test_that("as_cells() works with all common datatypes", {
  x <- tibble::tibble(
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
    list = list(1:2, letters[1:2])
  )
  y <- as_cells(x, col_names = TRUE)
  expect_equal(colnames(y), c("row", "col", "data_type", sort(colnames(x))))
  x_class <- purrr::map(x, class)
  y_class <- purrr::map(y, class)
  expect_equal(y_class[names(x_class)], x_class)
  # Separate test for factors, ordered factors and lists
  x <- tibble::tibble(
    fct = factor(c("a", "b")),
    ord = factor(c("c", "d"), ordered = TRUE),
    list = list(1:2, list("a", "b"))
  )
  y <- as_cells(x)
  expect_equal(colnames(y), c("row", "col", "data_type", sort(colnames(x))))
  expect_equal(class(y$fct), "list")
  expect_equal(class(y$ord), "list")
  expect_equal(class(y$list), "list")
  expect_equal(y$fct[[1]], factor("a", levels = c("a", "b")))
  expect_equal(y$fct[[3]], NULL)
  expect_equal(y$ord[[2]], NULL)
  expect_equal(y$ord[[4]], factor("d", levels = c("c", "d"), ordered = TRUE))
  expect_equal(y$list[[4]], NULL)
  expect_equal(y$list[[5]], 1:2)
  expect_equal(y$list[[6]], list("a", "b"))
})
