#' Add formatting information from the indenting format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' indenting formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

indenting <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_indenting,NA_real_),sheet_format = sheet_format) %>% unlist %>% unlist
}


#' Add formatting information from the text_color format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' text_color formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

text_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_text_color,NA_real_),sheet_format = sheet_format) %>% unlist
}


#' Add formatting information from the bg_color format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' bg_color formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

bg_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_bg_color,NA_real_),sheet_format = sheet_format) %>% unlist
}


#' Add formatting information from the h_alignment format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' h_alignment formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

h_alignment <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_h_alignment,NA_real_),sheet_format = sheet_format) %>% unlist
}

#' Add formatting information from the fmt_numFmt format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' numFmt formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_numFmt <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_numFmt_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_bold format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_bold formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_bold <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_bold_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_italic format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_italic formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_italic <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_italic_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_underline format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_underline formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_underline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_underline_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_font_strike format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_strike formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_strike <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_strike_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_font_vertAlign format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_vertAlign formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_vertAlign <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_vertAlign_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_font_size format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_size formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_size <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_size_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_font_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_font_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_name format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_name formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_name <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_name_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_family format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_family formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_family <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_family_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_font_scheme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' font_scheme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_scheme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_scheme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_fill_patternFill_fgColor_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_fgColor_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export
fmt_fill_patternFill_fgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_fill_patternFill_fgColor_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_fgColor_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_patternFill_fgColor_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_fgColor_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_patternFill_fgColor_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_fgColor_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_patternFill_bgColor_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_bgColor_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_fill_patternFill_bgColor_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_bgColor_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_patternFill_bgColor_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_bgColor_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_fill_patternFill_bgColor_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_bgColor_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Add formatting information from the fmt_fill_patternFill_patternType format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_patternFill_patternType formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_patternType <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_patternType_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_fill_gradientFill_type format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_type formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_type <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_type_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_degree format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_degree formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_degree <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_degree_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_left format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_left formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_left <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_left_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_right format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_right formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_right <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_right_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_top format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_top formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_top <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_top_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_fill_gradientFill_bottom format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_bottom formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_bottom <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_bottom_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop1_position format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop1_position formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_position_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop1_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop1_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_fill_gradientFill_stop1_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop1_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_fill_gradientFill_stop1_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop1_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop1_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop1_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop2_position format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop2_position formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_position_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Add formatting information from the fmt_fill_gradientFill_stop2_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop2_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_fill_gradientFill_stop2_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop2_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop2_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop2_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_fill_gradientFill_stop2_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' fill_gradientFill_stop2_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_diagonalDown format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonalDown formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonalDown <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonalDown_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_diagonalUp format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonalUp formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonalUp <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonalUp_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_outline format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_outline formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_outline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_outline_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_left_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_left_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_left_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_left_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_left_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_left_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_left_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_left_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_left_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_left_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Add formatting information from the fmt_border_right_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_right_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_right_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_right_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_right_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_right_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_right_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_right_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_right_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_right_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_start_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_start_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_start_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_start_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_start_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_start_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_start_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_start_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_start_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_start_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_end_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_end_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_end_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_end_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_end_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_end_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_end_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_end_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_end_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_end_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_top_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_top_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_top_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_top_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_top_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_top_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_top_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_top_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_top_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_top_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_bottom_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_bottom_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_bottom_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_bottom_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_bottom_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_bottom_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }





#' Add formatting information from the fmt_border_bottom_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_bottom_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_bottom_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_bottom_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_diagonal_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonal_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_diagonal_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonal_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Add formatting information from the fmt_border_diagonal_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonal_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_diagonal_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonal_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_diagonal_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_diagonal_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_vertical_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_vertical_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_vertical_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_vertical_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_vertical_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_vertical_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_border_vertical_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_vertical_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Add formatting information from the fmt_border_vertical_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_vertical_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_horizontal_style format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_horizontal_style formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_horizontal_color_rgb format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_horizontal_color_rgb formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_horizontal_color_theme format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_horizontal_color_theme formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_border_horizontal_color_indexed format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_horizontal_color_indexed formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export



fmt_border_horizontal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_border_horizontal_color_tint format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' border_horizontal_color_tint formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_alignment_horizontal format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_horizontal formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_horizontal <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_horizontal_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_alignment_vertical format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_vertical formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_vertical <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_vertical_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_alignment_wrapText format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_wrapText formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_wrapText <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_wrapText_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_alignment_readingOrder format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_readingOrder formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_readingOrder <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_readingOrder_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_alignment_indent format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_indent formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_indent <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_indent_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_alignment_justifyLastLine format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_justifyLastLine formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_justifyLastLine <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_justifyLastLine_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_alignment_shrinkToFit format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_shrinkToFit formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_shrinkToFit <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_shrinkToFit_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Add formatting information from the fmt_alignment_textRotation format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' alignment_textRotation formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_textRotation <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_textRotation_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Add formatting information from the fmt_protection_locked format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' protection_locked formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_protection_locked <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_protection_locked_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Add formatting information from the fmt_protection_hidden format object
#' This function uses the format object created by `xlsx_formats` along with `local_format_id`` to create a vector representing cells' protection_hidden formatting.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_protection_hidden <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_protection_hidden_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




















