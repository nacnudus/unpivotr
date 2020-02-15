
get_one <- function(format_id= local_format_id){
  1L
}

ones <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% purrr::map_int(purrr::possibly(get_one,NA_real_))
}


get_two <- function(format_id= local_format_id){
  2L
}

twos <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% purrr::map_int(purrr::possibly(get_two,NA_real_))
}


into_list <- function(x,y){
  x[[y]]
}



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
    
    
    format_id_vec %>% map(purrr::possibly(fmt_alignment_indent_single,NA_real_),sheet_formats = sheet_formats) %>% unlist 
    
    }


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









fmt_numFmt_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('numFmt')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_bold_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','bold')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}


fmt_font_italic_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','italic')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}


fmt_font_underline_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','underline')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_strike_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','strike')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_vertAlign_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','vertAlign')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_size_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','size')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_font_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_font_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_name_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','name')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_family_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','family')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_font_scheme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('font','scheme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_fgColor_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_fgColor_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_fgColor_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_fgColor_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','fgColor','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_bgColor_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_bgColor_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_patternFill_bgColor_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_patternFill_bgColor_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','bgColor','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}





fmt_fill_patternFill_patternType_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','patternFill','patternType')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_type_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','type')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_degree_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','degree')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_fill_gradientFill_left_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','left')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_right_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','right')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_top_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','top')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_bottom_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','bottom')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop1_position_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','position')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop1_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop1_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop1_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop1_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop1','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop2_position_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','position')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop2_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop2_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop2_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_fill_gradientFill_stop2_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('fill','gradientFill','stop2','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_diagonalDown_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonalDown')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_diagonalUp_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonalUp')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_outline_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','outline')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_left_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_left_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_left_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_left_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_left_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','left','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}





fmt_border_right_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_right_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_right_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_right_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_right_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','right','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_start_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_start_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_start_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_start_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','start','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_end_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_end_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_end_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_end_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_end_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','end','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_top_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_top_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_top_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_top_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_top_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','top','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_bottom_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_bottom_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_bottom_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_bottom_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_bottom_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','bottom','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_diagonal_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_diagonal_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_diagonal_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_border_diagonal_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','diagonal','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_vertical_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_vertical_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_vertical_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_vertical_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}





fmt_border_vertical_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','vertical','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_horizontal_style_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','style')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_horizontal_color_rgb_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','rgb')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_horizontal_color_theme_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','theme')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_horizontal_color_indexed_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','indexed')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_border_horizontal_color_tint_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('border','horizontal','color','tint')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_horizontal_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','horizontal')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_vertical_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','vertical')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_wrapText_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','wrapText')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_readingOrder_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','readingOrder')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_indent_single <- 
  function(local_format_id,sheet_formats = formats){
    
    
    format_type_vec <- c('alignment','indent')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_justifyLastLine_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','justifyLastLine')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_alignment_shrinkToFit_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','shrinkToFit')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_alignment_textRotation_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('alignment','textRotation')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}




fmt_protection_locked_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('protection','locked')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}



fmt_protection_hidden_single <- 
  function(local_format_id,sheet_formats = formats){
    format_type_vec <- c('protection','hidden')
    append(list(sheet_formats),c("local",format_type_vec)) %>%  purrr::reduce(into_list) %>% .[[local_format_id]]}












