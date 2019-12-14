#' Format function1
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

indenting <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_indenting,NA_real_),sheet_format = sheet_format) %>% unlist %>% unlist
}


#' Format function2
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

text_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_text_color,NA_real_),sheet_format = sheet_format) %>% unlist
}


#' Format function3
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

bg_color <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_bg_color,NA_real_),sheet_format = sheet_format) %>% unlist
}


#' Format function4
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_format formats 
#'
#'
#' @export

h_alignment <- function(format_id_vec= local_format_id,sheet_format = formats){
  format_id_vec %>% map(purrr::possibly(get_h_alignment,NA_real_),sheet_format = sheet_format) %>% unlist
}

#' Format function5
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_numFmt <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_numFmt_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function6
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_bold <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_bold_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function7
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_italic <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_italic_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function8
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_underline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_underline_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function9
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_strike <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_strike_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function10
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_vertAlign <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_vertAlign_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function11
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_size <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_size_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function12
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function13
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function14
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function15
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function16
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_name <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_name_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function17
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export


fmt_font_family <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_family_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function18
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_font_scheme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_font_scheme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function19
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export
fmt_fill_patternFill_fgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function20
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function21
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function22
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_fgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_fgColor_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function23
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function24
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function25
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function26
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_bgColor_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_bgColor_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Format function27
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_patternFill_patternType <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_patternFill_patternType_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function28
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_type <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_type_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function29
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_degree <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_degree_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function30
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_left <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_left_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function31
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_right <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_right_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function32
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_top <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_top_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function33
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_bottom <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_bottom_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function34
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_position_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function35
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function36
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function37
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function38
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop1_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop1_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function39
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_position <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_position_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Format function40
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function41
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function42
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function43
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_fill_gradientFill_stop2_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_fill_gradientFill_stop2_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function44
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonalDown <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonalDown_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function45
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonalUp <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonalUp_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function46
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_outline <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_outline_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function47
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function48
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function49
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function50
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function51
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_left_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_left_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Format function52
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function53
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function54
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function55
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function56
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_right_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_right_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function57
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function58
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function59
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function60
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function61
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_start_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_start_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function62
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function63
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function64
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function65
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function66
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_end_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_end_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function67
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function68
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function69
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function70
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function71
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_top_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_top_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function72
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function73
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function74
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }





#' Format function75
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function76
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_bottom_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_bottom_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function77
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function78
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Format function79
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function80
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function81
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_diagonal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_diagonal_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function82
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function83
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function84
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function85
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




#' Format function86
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_vertical_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_vertical_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function87
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_style <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_style_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function88
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_rgb <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_rgb_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function89
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_theme <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_theme_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function90
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export



fmt_border_horizontal_color_indexed <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_indexed_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function91
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_border_horizontal_color_tint <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_border_horizontal_color_tint_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function92
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_horizontal <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_horizontal_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function93
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_vertical <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_vertical_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function94
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_wrapText <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_wrapText_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function95
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_readingOrder <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_readingOrder_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function96
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_indent <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_indent_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function97
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_justifyLastLine <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_justifyLastLine_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function98
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_shrinkToFit <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_shrinkToFit_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }

#' Format function99
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_alignment_textRotation <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_alignment_textRotation_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }


#' Format function100
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_protection_locked <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_protection_locked_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }



#' Format function101
#' Produces the  tidyABS table components, which store information on column groups, row groups and tabledata.
#' @param format_id_vec local format id vector 
#' @param sheet_formats formats 
#'
#'
#' @export

fmt_protection_hidden <- 
  function(format_id_vec= local_format_id,sheet_formats = formats){
    format_id_vec %>% map(purrr::possibly(fmt_protection_hidden_single,NA_real_),sheet_formats = sheet_formats) %>% unlist }




















