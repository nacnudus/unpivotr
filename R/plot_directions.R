
#' plot orientations
#'
#' This function plots the excel sheet, highlighting the relationship between headers and table values.
#' @param orientated_df returned by orientated or orientate_df
#'
#' @examples
#'
#' @export

plot_cells <- function(sheet, text = values, interactive = FALSE) {

  data_cells <-
    sheet %>%
    attr("data_cells") %>%
    mutate(.direction = "\U2610", .header_label = "data")
  
  
    
  if (interactive == FALSE) {
    sheet_01 <-
      bind_rows(sheet, data_cells) %>%
      mutate(.arrow = case_when(
        .direction == "N"   ~ "\U2193",
        .direction == "NNE" ~ "\U21B1", 
        .direction == "ENE" ~ "\U21B3",
        .direction == "E"   ~ "\U2190",
        .direction == "ESE" ~ "\U21B2",
        .direction == "SSE" ~ "\U21B0",     
        .direction == "S"   ~ "\U2191",
        .direction == "SSW" ~ "\U21B1",
        .direction == "WSW" ~ "\U21B0",
        .direction == "W"   ~ "\U2192",
        .direction == "WNW" ~ "\U21B1",
        .direction == "NNW" ~ "\U21B3")) %>% 
      mutate(values = coalesce(as.character(numeric),as.character(character),
                               as.character(logical),as.character(date)))
    
    bind_rows(
      mutate(sheet_01, .arrow = NA, set = "Cell values"),
      mutate(sheet_01, {{text}} := NA, set = "Directions")) %>%
      ggplot(aes(x = col, y = -row, fill = .header_label)) + geom_tile() +
      geom_text(aes(label = str_sub({{text}}, 1, 5))) +
      geom_text(aes(label = ifelse(!.direction %in% c("NNE","ESE","WSW","ENE"), .arrow, NA))) +
      geom_text(aes(label = ifelse(.direction %in% c("NNE","ESE","WSW"), .arrow, NA)), angle = 270) +
      geom_text(aes(label = ifelse(.direction %in% c("ENE"), .arrow, NA)), angle = 270) +
      facet_wrap(~set, scales = "free")
  } else {
    
    sheet_01 <-
      bind_rows(sheet, data_cells) %>%
      mutate(.arrow = case_when(
        .direction == "N"   ~ "\U2193",
        .direction == "NNE" ~ "\U21B1", 
        .direction == "ENE" ~ "\U21B1",
        .direction == "E"   ~ "\U2190",
        .direction == "ESE" ~ "\U21B2",
        .direction == "SSE" ~ "\U21B0",     
        .direction == "S"   ~ "\U2191",
        .direction == "SSW" ~ "\U21B1",
        .direction == "WSW" ~ "\U21B0",
        .direction == "W"   ~ "\U2192",
        .direction == "WNW" ~ "\U21B1",
        .direction == "NNW" ~ "\U21B3")) %>%  
      mutate(values = coalesce(as.character(numeric),as.character(character),
                               as.character(logical),as.character(date)))
    
    plot_object <-
      bind_rows(
        mutate(sheet_01, .arrow = NA, set = "Cell values"),
        mutate(sheet_01, {{text}} := NA, set = "Directions")
      ) %>%
      ggplot(aes(x = col, y = -row, fill = .header_label)) + geom_tile() +
      geom_text(aes(label = str_sub({{text}}, 1, 5))) +
      geom_text(aes(label = ifelse(.direction %in% c("W", "N"), .arrow, NA))) +
      geom_text(aes(label = ifelse(.direction %in% c("WNW"), .arrow, NA)), angle = -90) +
      facet_wrap(~set, scales = "free")
    
    ggplotly(plot_object)
  }
}
