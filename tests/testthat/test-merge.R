context("test-merge")

x <- tibble::tribble(
  ~ row, ~ col, ~ data_type,     ~ chr,
      1,     1,       "chr",    "Katy",
      2,     1,       "chr",   "Perry",
      3,     1,       "chr",       "a",
      4,     1,       "chr",       "b",
      5,     1,       "chr",       "c",
      2,     2,       "chr",   "Adele",
      3,     2,       "chr",       "d",
      4,     2,       "chr",       "e",
      5,     2,       "chr",       "f",
      1,     3,       "chr",  "Ariana",
      2,     3,       "chr",  "Grande",
      3,     3,       "chr",       "g",
      4,     3,       "chr",       "h",
      5,     3,       "chr",       "i"
  )

y <- tibble::tribble(
  ~ col, ~ row, ~ data_type,            ~ chr,
      1,     1,       "chr",     "Katy Perry",
      2,     1,       "chr",          "Adele",
      3,     1,       "chr",  "Ariana Grande",
      1,     3,       "chr",              "a",
      1,     4,       "chr",              "b",
      1,     5,       "chr",              "c",
      2,     3,       "chr",              "d",
      2,     4,       "chr",              "e",
      2,     5,       "chr",              "f",
      3,     3,       "chr",              "g",
      3,     4,       "chr",              "h",
      3,     5,       "chr",              "i"
  )

z <- tibble::tribble(
  ~ row, ~ col, ~ data_type,          ~ chr,
      1,     1,       "chr",         "Katy",
      2,     1,       "chr",  "Perry Adele",
      3,     1,       "chr",          "a d",
      4,     1,       "chr",          "b e",
      5,     1,       "chr",          "c f",
      1,     3,       "chr",       "Ariana",
      2,     3,       "chr",       "Grande",
      3,     3,       "chr",            "g",
      4,     3,       "chr",            "h",
      5,     3,       "chr",            "i"
  )

test_that("merge_rows works", {
  expect_equal(merge_rows(x, 1:2, chr), y)
})

test_that("merge_cols works", {
  expect_equal(merge_cols(x, 1:2, chr), z)
})
