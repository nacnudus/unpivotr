context("test-behead.R")

x <- purpose$`NNW WNW`
cells <- tidy_table(x)

test_that("behead() works", {
  # Strip the headers and make them into data
  tidy <-
    cells %>%
    behead(NNW, Sex, chr) %>%
    behead(N, `Sense of purpose`, chr) %>%
    behead(WNW, `Highest qualification`, chr) %>%
    behead(W, `Age group (Life-stages)`, chr) %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(dplyr::anti_join(tidy, purpose$Tidy)), 1)
  # The provided 'tidy' data is missing a roww for Male 15-24-year-olds with a
  # postgraduate qualification and a sense of purpose between 0 and 6.  That
  # seems to have been an oversight by Statistics New Zealand.
})

test_that("the `drop_na` argument of behead() works", {
  # Strip the headers and make them into data
  tidy <-
    cells %>%
    behead(N, Sex, chr, drop_na = FALSE) %>%
    behead(N, `Sense of purpose`, chr) %>%
    behead(WNW, `Highest qualification`, chr) %>%
    behead(W, `Age group (Life-stages)`, chr) %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(tidy), 80)
  expect_equal(tidy$Sex, rep(c("Female", NA, "Male", NA), each = 20))
})
