
#' plot orientations
#'
#' This function plots the excel sheet, highlighting the relationship between headers and table values.
#' @param orientated_df returned by orientated or orientate_df
#'
#' @examples
#'
#' @export

plot_directions <- function(sheet, interactive = FALSE) {
  sheet <- plot_data

  data_cells <-
    sheet %>%
    attr("data_cells") %>%
    mutate(.direction = "\U2610", .header_label = "data")


  if (interactive == FALSE) {
    sheet_01 <-
      bind_rows(sheet, data_cells) %>%
      mutate(.arrow = case_when(
        .direction == "W" ~ "\U2192",
        .direction == "N" ~ "\U2193",
        .direction == "NNW" ~ "\U21B1",
        .direction == "WNW" ~ "\U21B1",
        T ~ NA_character_
      ))

    bind_rows(
      mutate(sheet_01, .arrow = NA, set = "Cell values"),
      mutate(sheet_01, .value = NA, set = "Directions")
    ) %>%
      ggplot(aes(x = col, y = -row, fill = .header_label)) + geom_tile() +
      geom_text(aes(label = str_sub(.value, 1, 5))) +
      geom_text(aes(label = ifelse(.direction %in% c("W", "N"), .arrow, NA))) +
      geom_text(aes(label = ifelse(.direction %in% c("NNW", "WNW"), .arrow, NA)), angle = -90) +
      facet_wrap(~set, scales = "free")
  } else {
    sheet_01 <-
      bind_rows(sheet, data_cells) %>%
      mutate(.arrow = case_when(
        .direction == "W" ~ "\U2192",
        .direction == "N" ~ "\U2193",
        .direction == "NNW" ~ "\U21B4",
        .direction == "WNW" ~ "\U21B4",
        T ~ NA_character_
      ))

    plot_object <-
      bind_rows(
        mutate(sheet_01, .arrow = NA, set = "Cell values"),
        mutate(sheet_01, .value = NA, set = "Directions")
      ) %>%
      ggplot(aes(x = col, y = -row, fill = .header_label)) + geom_tile() +
      geom_text(aes(label = str_sub(.value, 1, 5))) +
      geom_text(aes(label = ifelse(.direction %in% c("W", "N"), .arrow, NA))) +
      geom_text(aes(label = ifelse(.direction %in% c("NNW", "WNW"), .arrow, NA)), angle = -90) +
      facet_wrap(~set, scales = "free")

    ggplotly(plot_object)
  }
}
