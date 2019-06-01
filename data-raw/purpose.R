library(tidyverse)
library(xlsx)
library(tidyxl)
library(unpivotr)
path <- "./inst/extdata/purpose.xlsx"
sheets <-
  path %>%
  loadWorkbook() %>%
  getSheets() %>%
  names() %>%
  .[5:10]
purpose <- lapply(
  sheets,
  function(x) {
    read.xlsx(path, x, header = FALSE, stringsAsFactors = FALSE)
  }
)
names(purpose) <- sheets
tidy <- read.xlsx(path, "Tidy", stringsAsFactors = FALSE, check.names = FALSE)
small_multiples <-
  xlsx_cells(path, "Small multiples") %>%
  select(row, col, data_type, character, numeric) %>%
  as.data.frame()
purpose <- c(
  list(Tidy = tidy),
  purpose,
  list(small_multiples = small_multiples)
)
usethis::use_data(purpose, overwrite = TRUE)
