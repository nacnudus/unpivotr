context("test-behead.R")

x <- purpose$`NNW WNW`
cells <- tidy_table(x)

test_that("behead() works", {
  # Strip the headers and make them into data
  tidy <-
    cells %>%
    behead(NNW, "Sex") %>%
    behead(N, "Sense of purpose") %>%
    behead(WNW, "Highest qualification") %>%
    behead(W, "Age group (Life-stages)") %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(dplyr::anti_join(tidy, purpose$Tidy)), 1)
  # The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
  # postgraduate qualification and a sense of purpose between 0 and 6.  That
  # seems to have been an oversight by Statistics New Zealand.
})

test_that("the `drop_na` argument of behead() works", {
  # Strip the headers and make them into data
  tidy <-
    cells %>%
    behead(N, "Sex", drop_na = FALSE) %>%
    behead(N, "Sense of purpose") %>%
    behead(WNW, "Highest qualification") %>%
    behead(W, "Age group (Life-stages)") %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(tidy), 80)
  expect_equal(tidy$Sex, rep(c("Female", NA, "Male", NA), each = 20))
})

test_that("``\"ABOVE\"`` etc. don't work", {
  error_message <- "`direction` must be one of \"NNW\", \"N\", \"NNE\", \"ENE\", \"E\", \"ESE\", \"SSE\", \"S\", \"SSW\", \"WSW\", \"W\", \"WNW\""
  # Strip the headers and make them into data
  expect_error(behead(cells, ABOVE, "Sex"), error_message)
  expect_error(behead(cells, FOO, "Sex"), error_message)
})

test_that("behead() works with all common datatypes", {
  sysdate <- Sys.Date()
  systime <- Sys.time()
  x <-
    data.frame(lgl = c(TRUE, FALSE),
               int = c(1L, 2L),
               dbl = c(1, 2),
               cpl = c(1i, 2i),
               raw = as.raw(c(11, 12)),
               date = c(sysdate, sysdate + 1),
               dttm = c(systime, systime + 1),
               chr = c("a", "b"),
               fct = factor(c("c", "d")),
               ord = factor(c("e", "f"), ordered = TRUE),
               stringsAsFactors = FALSE) %>%
    tidy_table(colnames = TRUE)
  y <- behead(x, N, header)
  expect_equal(y$chr[14], NA_character_)
  expect_equal(y$chr[15], "a")
  expect_equal(y$cpl[6], NA_complex_)
  expect_equal(y$cpl[7], 0+1i)
  expect_equal(y$date[10], as.Date(NA))
  expect_equal(y$date[11], sysdate)
  expect_equal(y$dbl[4], NA_real_)
  expect_equal(y$dbl[5], 1)
  expect_equal(y$dttm[12], as.POSIXct(NA))
  expect_equal(y$dttm[13], systime)
  expect_equal(y$fct[[16]], NULL)
  expect_equal(y$int[2], NA_integer_)
  expect_equal(y$int[3], 1L)
  expect_equal(y$lgl[2], FALSE)
  expect_equal(y$lgl[3], NA)
  expect_equal(y$ord[[18]], NULL)
  expect_equal(y$ord[[19]], factor("e", levels = c("e", "f"), ordered = TRUE))
  expect_equal(y$raw[[8]], as.raw(0))
  expect_equal(y$raw[[9]], as.raw(11))
})

test_that("behead() handles headers of mixed data types including dates", {
  x <- data.frame(row = c(1L, 1L, 2L, 2L, 3L, 3L),
                  col = c(1L, 2L, 1L, 2L, 1L, 2L),
                  data_type = c("dttm", "date", "chr", "dbl", "chr", "dbl"),
                  chr = c(NA, NA, "Matilda", NA, "Nicholas", NA),
                  date = as.Date(c(NA, "2000-01-01", rep(NA, 4))),
                  dttm = as.POSIXct(c("2001-01-01 11:00:00", rep(NA, 5))),
                  dbl = c(NA, NA, NA, 11, NA, 12),
                  stringsAsFactors = FALSE)
  y <- behead(x, N, header)
  expect_equal(y$header, rep(c("2001-01-01 11:00:00", "2000-01-01"), 2))
})

test_that("behead() handles headers of factor and ordered-factor data types", {
  x <-
    tibble(row = c(1L, 1L, 2L, 2L, 3L, 3L),
           col = c(1L, 2L, 1L, 2L, 1L, 2L),
           data_type = c("fct", "ord", "chr", "dbl", "chr", "dbl"),
           chr = c(NA, NA, "Matilda", NA, "Nicholas", NA),
           fct = list(factor("name"), NULL, NULL, NULL, NULL, NULL),
           ord = list(NULL, factor("score", ordered = TRUE),
                      NULL, NULL, NULL, NULL),
           date = as.Date(c(NA, "2000-01-01", rep(NA, 4))),
           dbl = c(NA, NA, NA, 11, NA, 12))
  y <- behead(x, N, header)
  expect_equal(y$header, rep(c("name", "score"), 2))
})

test_that("behead() supports custom formatters", {
  x <-
    tidy_table(BOD, FALSE, TRUE) %>%
    behead(N, header, chr = ~ paste(.x, "foo")) %>%
    behead(W, rowheader, dbl = as.complex)
  expect_equal(x$header[1], "demand foo")
  expect_equal(x$rowheader[1], 1+0i)
})

test_that("behead() can use row, col and data_type as headers", {
  x <- tidy_table(BOD, FALSE, TRUE)
  y <- behead(x, N, header, values = row)
  expect_equal(y$header, rep(1L, 12L))
  expect_equal(colnames(y), c(colnames(x), "header"))
  y <- behead(x, N, header, values = col)
  expect_equal(y$header, rep(1:2, each = 6L))
  expect_equal(colnames(y), c(colnames(x), "header"))
  y <- behead(x, N, header, values = data_type)
  expect_equal(y$header, rep("chr", 12L))
  expect_equal(colnames(y), c(colnames(x), "header"))
})

