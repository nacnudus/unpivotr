context("test-behead.R")

x <- purpose$`up-left left-up`
cells <- as_cells(x)

test_that("behead() works", {
  # Strip the headers and make them into data
  tidy <-
    cells %>%
    behead("NNW", "Sex") %>%
    behead("N", "Sense of purpose") %>%
    behead("WNW", "Highest qualification") %>%
    behead("W", "Age group (Life-stages)") %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(dplyr::anti_join(tidy, purpose$Tidy)), 1)
  # The provided 'tidy' data is missing a row for Male 15-24-year-olds with a
  # postgraduate qualification and a sense of purpose between 0 and 6.  That
  # seems to have been an oversight by Statistics New Zealand.
})

test_that("the `drop_na` argument of behead() works", {
  tidy <-
    cells %>%
    behead("NNW", "Sex", drop_na = TRUE) %>%
    behead("N", "Sense of purpose") %>%
    behead("WNW", "Highest qualification") %>%
    behead("W", "Age group (Life-stages)") %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(tidy), 80)
  expect_equal(tidy$Sex, rep(rep(c("Female", "Male"), each = 8), 5))
  tidy <-
    cells %>%
    behead("NNW", "Sex", drop_na = FALSE) %>%
    behead("N", "Sense of purpose") %>%
    behead("WNW", "Highest qualification") %>%
    behead("W", "Age group (Life-stages)") %>%
    dplyr::select(-row, -col, -data_type, -chr)
  # Check against the provided 'tidy' version of the data.
  expect_equal(nrow(tidy), 80)
  expect_equal(tidy$Sex, rep(rep(c("Female", NA, "Male", NA), each = 4), 5))
})

test_that("``\"ABOVE\"`` etc. don't work", {
  error_message <-"To use the directions \"up-ish\", \"right-ish\", \"down-ish\", \"left-ish\" look at `\\?enhead`."
  # Strip the headers and make them into data
  expect_error(behead(cells, "up-ish", "Sex"),
               "To use the directions \"up-ish\", \"right-ish\", \"down-ish\", \"left-ish\" look at `\\?enhead`.")
})

test_that("behead() works with all common datatypes", {
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
  x <- as_cells(w, col_names = TRUE)
  y <- behead(x, "up", header)
  expect_equal(nrow(y), 20L)
  expect_equal(y$header, rep(colnames(w), each = 2L))
  expect_equal(y$chr[12], NA_character_)
  expect_equal(y$chr[13], "a")
  expect_equal(y$cpl[6], NA_complex_)
  expect_equal(y$cpl[7], 0 + 1i)
  expect_equal(y$date[8], as.Date(NA))
  expect_equal(y$date[9], as.Date("2001-01-01"))
  expect_equal(y$dbl[4], NA_real_)
  expect_equal(y$dbl[5], 1)
  expect_equal(y$dttm[10], as.POSIXct(NA))
  expect_equal(y$dttm[11], as.POSIXct("2001-01-01 01:01:01"))
  expect_equal(y$fct[[16]], factor("d", levels = c("c", "d")))
  expect_equal(y$fct[[17]], NULL)
  expect_equal(y$int[2], NA_integer_)
  expect_equal(y$int[3], 1L)
  expect_equal(y$lgl[2], FALSE)
  expect_equal(y$lgl[3], NA)
  expect_equal(y$ord[[18]], factor("f", levels = c("e", "f"), ordered = TRUE))
  expect_equal(y$ord[[19]], NULL)
  expect_equal(y$list[[18]], NULL)
  expect_equal(y$list[[19]], 1:2)
  expect_equal(y$list[[20]], letters[1:2])
})

test_that("behead() handles headers of mixed data types including dates", {
  x <- data.frame(
    row = c(1L, 1L, 2L, 2L, 3L, 3L),
    col = c(1L, 2L, 1L, 2L, 1L, 2L),
    data_type = c("dttm", "date", "chr", "dbl", "chr", "dbl"),
    chr = c(NA, NA, "Matilda", NA, "Nicholas", NA),
    date = as.Date(c(NA, "2000-01-01", rep(NA, 4))),
    dttm = as.POSIXct(c("2001-01-01 11:00:00", rep(NA, 5))),
    dbl = c(NA, NA, NA, 11, NA, 12),
    stringsAsFactors = FALSE
  )
  y <- behead(x, "N", header)
  expect_equal(y$header, rep(c("2001-01-01 11:00:00", "2000-01-01"), 2))
})

test_that("behead() handles headers of factor and ordered-factor data types", {
  x <-
    tibble::tibble(
      row = c(1L, 1L, 2L, 2L, 3L, 3L),
      col = c(1L, 2L, 1L, 2L, 1L, 2L),
      data_type = c("fct", "ord", "chr", "dbl", "chr", "dbl"),
      chr = c(NA, NA, "Matilda", NA, "Nicholas", NA),
      fct = list(factor("name"), NULL, NULL, NULL, NULL, NULL),
      ord = list(
        NULL, factor("score", ordered = TRUE),
        NULL, NULL, NULL, NULL
      ),
      date = as.Date(c(NA, "2000-01-01", rep(NA, 4))),
      dbl = c(NA, NA, NA, 11, NA, 12)
    )
  y <- behead(x, "N", header)
  expect_equal(y$header, rep(c("name", "score"), 2))
})

test_that("behead() supports custom formatters", {
  x <-
    as_cells(BOD, FALSE, TRUE) %>%
    behead("up", header, formatters = list(chr = ~ paste(.x, "foo"))) %>%
    behead("left", rowheader, formatters = list(dbl = as.complex))
  expect_equal(x$header[1], "demand foo")
  expect_equal(x$rowheader[1], 1 + 0i)
})

test_that("behead() can use row, col and data_type as headers", {
  x <- as_cells(BOD, FALSE, TRUE)
  y <- behead(x, "up", header, values = row)
  expect_equal(y$header, rep(1L, 12L))
  expect_equal(colnames(y), c(colnames(x), "header"))
  y <- behead(x, "up", header, values = col)
  expect_equal(y$header, rep(1:2, each = 6L))
  expect_equal(colnames(y), c(colnames(x), "header"))
  y <- behead(x, "up", header, values = data_type)
  expect_equal(y$header, rep("chr", 12L))
  expect_equal(colnames(y), c(colnames(x), "header"))
})

test_that("behead() stops on non-distinct cells", {
  expect_error(
    behead(dplyr::bind_rows(cells, cells), "NNW"),
    "Row and column numbers must be distinct.\n  Perhaps you meant to use a single sheet.",
    fixed = TRUE
  )
})

test_that("behead_if() works", {
  cells <- tibble::tribble(
        ~ X1, ~ adult, ~ juvenile,
      "LION",     855,        677,
      "male",     496,        322,
    "female",     359,        355,
     "TIGER",     690,        324,
      "male",     381,        222,
    "female",     309,        102
    )
  cells <- as_cells(cells, col_names = TRUE)
  x <-
    cells %>%
    behead_if(chr == toupper(chr), direction = "WNW", name = "species") %>%
    behead("left", "sex") %>%
    behead("up", "age") %>%
    dplyr::select(species, sex, age, population = dbl)
  y <- tibble::tribble(
    ~ species,     ~ sex,       ~ age, ~ population,
       "LION",        NA,     "adult",          855,
       "LION",    "male",     "adult",          496,
       "LION",  "female",     "adult",          359,
       "LION",        NA,  "juvenile",          677,
       "LION",    "male",  "juvenile",          322,
       "LION",  "female",  "juvenile",          355,
      "TIGER",        NA,     "adult",          690,
      "TIGER",    "male",     "adult",          381,
      "TIGER",  "female",     "adult",          309,
      "TIGER",        NA,  "juvenile",          324,
      "TIGER",    "male",  "juvenile",          222,
      "TIGER",  "female",  "juvenile",          102
    )
  expect_equal(x, y)
})
