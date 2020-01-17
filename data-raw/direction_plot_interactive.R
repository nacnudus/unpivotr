library(usethis)

direction_plot_interactive <- 
structure(list(
  .direction = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"), 
  .arrow = c("↓", "↙", "↙", "←", "↖", "↖", "↑", "↗", "↗", "→",  "↘", "↘"), 
  .rotate = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), 
  row.names = c(NA, -12L), class = c("tbl_df", "tbl", "data.frame"))

usethis::use_data(direction_plot_interactive, overwrite = TRUE)

