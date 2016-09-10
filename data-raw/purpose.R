library(tidyxl)
library(purrr)
library(dplyr)
x <- contents("./inst/extdata/purpose.xlsx")
purpose <- 
  x %>%
  map(~ select(.x, row, col, integer = content, character)) %>%
  .[c(3, 5:10)]
use_data(purpose)

