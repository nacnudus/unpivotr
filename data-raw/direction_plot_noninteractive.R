library(usethis)

direction_plot_noninteractive <- 
structure(list(
  .direction = c("N", "NNW", "WNW", "W", "WSW","SSW", "S", "SSE", "ESE", "E", "ENE", "NNE"),
  .arrow = c("↓", "↳", "↱", "→", "↳", "↱", "↑", "↰", "↲", "←", "↰", "↲"), 
  .rotate = c(0, 0, -90, 0, 90, 0, 0, 0, -90, 0,90, 0)), 
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,-12L))


usethis::use_data(direction_plot_noninteractive, overwrite = TRUE)
