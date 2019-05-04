
#' Get formats
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param path path to .xlsx file
#' @param sheets sheet nominated for tidying
#'
#' @examples
#'
#'  \donttest{tidyABS_example("australian-industry.xlsx") %>% process_sheet(sheets = "Table_1")  }
#'
#'
#'
#' @export
 



get_indenting <- function(format_id = local_format_id,sheet_format = formats){
  sheet_format$local$alignment[["indent"]][[format_id]]
}

indenting <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_dbl(possibly(get_indenting,NA_real_),sheet_format = sheet_format)
}


get_bolding <- function(format_id= local_format_id,sheet_format = formats){
  sheet_format$local$font[["bold"]][[format_id]]
}

bolding <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_dbl(possibly(get_bolding,NA_real_),sheet_format = sheet_format)
}

get_italics <- function(format_id= local_format_id,sheet_format = formats){
  sheet_format$local$font[["italic"]][[format_id]]
}

italics <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_dbl(possibly(get_italics,NA_real_),sheet_format = sheet_format)
}


get_italics <- function(format_id= local_format_id,sheet_format = formats){
  sheet_format$local$font[["italic"]][[format_id]]
}

italics <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_dbl(possibly(get_italics,NA_real_),sheet_format = sheet_format)
}



get_text_color <- function(format_id= local_format_id,sheet_format = formats){
  formats$local$font$color$rgb[[format_id]]
}


text_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_chr(possibly(get_text_color,NA_real_),sheet_format = sheet_format)
}


get_bg_color <- function(format_id= local_format_id,sheet_format = formats){
  formats$local$fill$patternFill$bgColor[[format_id]]
}


bg_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_chr(possibly(get_bg_color,NA_real_),sheet_format = sheet_format)
}



get_h_alignment <- function(format_id= local_format_id,sheet_format = formats){
  formats$local$alignment$horizontal[[format_id]]
}


h_alignment <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_chr(possibly(get_h_alignment,NA_real_),sheet_format = sheet_format)
}



get_one <- function(format_id= local_format_id){
  1L
}

ones <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_int(possibly(get_one,NA_real_))
}


get_two <- function(format_id= local_format_id){
  2L
}

twos <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map_int(possibly(get_two,NA_real_))
}






fmt_numFmt_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('numFmt')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_numFmt <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_numFmt_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_bold_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','bold')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_bold <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_bold_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_italic_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','italic')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_italic <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_italic_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_underline_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','underline')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_underline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_underline_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_strike_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','strike')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_strike <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_strike_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_vertAlign_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','vertAlign')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_vertAlign <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_vertAlign_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_size_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','size')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_size <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_size_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_name_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','name')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_name <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_name_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_family_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','family')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_family <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_family_single,NA_real_),sheet_formats = sheet_formats)}



fmt_font_scheme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','scheme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_font_scheme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_font_scheme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_fgColor_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_fgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_fgColor_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_fgColor_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_fgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_fgColor_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_fgColor_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_fgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_fgColor_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_fgColor_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_fgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_fgColor_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_bgColor_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_bgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_bgColor_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_bgColor_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_bgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_bgColor_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_bgColor_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_bgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_bgColor_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_bgColor_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_bgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_bgColor_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_patternFill_patternType_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','patternType')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_patternType <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_patternFill_patternType_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_type_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','type')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_type <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_type_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_degree_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','degree')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_degree <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_degree_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_left_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','left')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_left <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_left_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_right_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','right')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_right <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_right_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_top_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','top')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_top <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_top_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_bottom_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','bottom')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_bottom <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_bottom_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop1_position_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','position')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop1_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop1_position_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop1_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop1_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop1_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop1_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop1_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop1_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop1_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop1_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop1_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop1_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop1_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop1_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop2_position_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','position')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop2_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop2_position_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop2_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop2_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop2_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop2_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop2_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop2_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop2_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop2_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop2_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_fill_gradientFill_stop2_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_stop2_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_fill_gradientFill_stop2_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonalDown_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonalDown')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonalDown <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonalDown_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonalUp_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonalUp')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonalUp <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonalUp_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_outline_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','outline')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_outline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_outline_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_left_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_left_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_left_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_left_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_left_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_left_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_left_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_left_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_left_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_left_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_right_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_right_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_right_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_right_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_right_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_right_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_right_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_right_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_right_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_right_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_start_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_start_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_start_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_start_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_start_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_start_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_start_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_start_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_start_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_start_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_end_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_end_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_end_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_end_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_end_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_end_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_end_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_end_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_end_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_end_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_top_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_top_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_top_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_top_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_top_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_top_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_top_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_top_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_top_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_top_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_bottom_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_bottom_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_bottom_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_bottom_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_bottom_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_bottom_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_bottom_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_bottom_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_bottom_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_bottom_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_bottom_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_bottom_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_bottom_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_bottom_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_bottom_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonal_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonal_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonal_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonal_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonal_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonal_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonal_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonal_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_diagonal_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_diagonal_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_vertical_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_vertical_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_vertical_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_vertical_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_vertical_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_vertical_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_vertical_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_vertical_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_vertical_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_vertical_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_vertical_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_vertical_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_vertical_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_vertical_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_vertical_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_horizontal_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_horizontal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_horizontal_style_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_horizontal_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_horizontal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_horizontal_color_rgb_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_horizontal_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_horizontal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_horizontal_color_theme_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_horizontal_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_horizontal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_horizontal_color_indexed_single,NA_real_),sheet_formats = sheet_formats)}



fmt_border_horizontal_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_border_horizontal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_border_horizontal_color_tint_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_horizontal_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','horizontal')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_horizontal <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_horizontal_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_vertical_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','vertical')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_vertical <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_vertical_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_wrapText_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','wrapText')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_wrapText <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_wrapText_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_readingOrder_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','readingOrder')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_readingOrder <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_readingOrder_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_indent_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','indent')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_indent <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_indent_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_justifyLastLine_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','justifyLastLine')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_justifyLastLine <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_justifyLastLine_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_shrinkToFit_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','shrinkToFit')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_shrinkToFit <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_shrinkToFit_single,NA_real_),sheet_formats = sheet_formats)}



fmt_alignment_textRotation_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','textRotation')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_textRotation <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_alignment_textRotation_single,NA_real_),sheet_formats = sheet_formats)}



fmt_protection_locked_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('protection','locked')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_protection_locked <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_protection_locked_single,NA_real_),sheet_formats = sheet_formats)}



fmt_protection_hidden_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('protection','hidden')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}



fmt_protection_hidden <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map_chr(possibly(fmt_protection_hidden_single,NA_real_),sheet_formats = sheet_formats)}
