
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


