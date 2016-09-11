library(xlsx)
path <- "./inst/extdata/purpose.xlsx"
sheets <- 
  path %>%
  loadWorkbook %>%
  getSheets %>%
  names %>%
  .[5:10]
purpose <- lapply(sheets,
            function(x) {
              read.xlsx(path, x, header = FALSE, stringsAsFactors = FALSE)
            })
names(purpose) <- sheets
tidy <- read.xlsx(path, "Tidy", stringsAsFactors = FALSE, check.names = FALSE)
purpose <- c(list(Tidy = tidy), purpose)
use_data(purpose, overwrite = TRUE)
