

get_indenting <- function(format_id,sheet_format){
  sheet_format$local$alignment[["indent"]][[format_id]]
}

get_indenting_vec <- function(format_id_vec,sheet_format){
  format_id_vec %>% map_dbl(possibly(get_indenting,NA_real_),sheet_format = sheet_format)
}


get_bolding <- function(format_id,sheet_format){
  sheet_format$local$font[["bold"]][[format_id]]
}

get_bolding_vec <- function(format_id_vec,sheet_format){
  format_id_vec %>% map_dbl(possibly(get_bolding,NA_real_),sheet_format = sheet_format)
}

get_italics <- function(format_id,sheet_format){
  sheet_format$local$font[["italic"]][[format_id]]
}

get_italics_vec <- function(format_id_vec,sheet_format){
  format_id_vec %>% map_dbl(possibly(get_italics,NA_real_),sheet_format = sheet_format)
}

as_cells <- function(x, row_names = FALSE, col_names = FALSE) {
  UseMethod("as_cells")
}

as_cells.data.frame <- function(x, row_names = FALSE, col_names = FALSE) {
  values <- do.call(c, purrr::map(x, as.list))
  nrows <- nrow(x)
  ncols <- ncol(x)
  types <- purrr::map_chr(x, pillar::type_sum)
  out <- tibble::tibble(
    row = rep.int(seq_len(nrow(x)), ncols),
    col = rep(seq_len(ncol(x)), each = nrows),
    value = values,
    type = rep(types, each = nrows),
    data_type = type
  )
  out <- tidyr::spread(out, type, value)
  if (row_names) {
    rnames <- row.names(x)
    out$col <- out$col + 1L
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        col = 1L,
        row = seq_along(rnames),
        chr = rlang::as_list(rnames),
        data_type = "chr"
      )
    )
  }
  if (col_names) {
    cnames <- colnames(x)
    out$row <- out$row + 1L
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        row = 1L,
        col = seq_along(cnames) + row_names,
        chr = rlang::as_list(cnames),
        data_type = "chr"
      )
    )
  }
  out <- dplyr::mutate_at(out,
    tidyselect::vars_select(
      names(out),
      dplyr::everything(),
      .exclude = c("row", "col", "data_type")
    ),
    concatenate,
    combine_factors = FALSE,
    fill_factor_na = FALSE
  )
  out <- dplyr::select(out, row, col, data_type, sort(colnames(out)))
  dplyr::arrange(out, col, row)
}

grow_matrix <- function(x, i, j, value) {
  dim_x <- dim(x)
  grow <- c(i, j) > dim_x
  if (any(grow)) {
    new_i <- if (grow[1]) dim_x[1] * 2 else dim_x[1]
    new_j <- if (grow[2]) dim_x[2] * 2 else dim_x[2]
    x <- do.call(
      what = `[<-`,
      args = list(matrix(value, new_i, new_j),
        i = seq_len(dim_x[1]),
        j = seq_len(dim_x[2]),
        value = x
      )
    )
    return(grow_matrix(x, i, j, value))
  }
  x
}

as_cells.xml_node <- function(x, row_names = FALSE, col_names = FALSE) {
  x <-
    x %>%
    as.character() %>%
    xml2::read_html() %>%
    xml2::xml_find_all(xpath = "//table[not(ancestor::table)]") %>%
    .[[1]]
  rows <- xml2::xml_find_all(x, xpath = "//tr[not(ancestor::tr)]")
  scratch <- matrix(FALSE, nrow = length(rows)) # For marking used cells
  out <- vector(mode = "list")
  i <- 0
  for (row_i in rows) {
    i <- i + 1
    if (is.null(out[i][[1]])) out[i][[1]] <- vector(mode = "character") # Create a row
    j <- 0
    cells <- xml2::xml_find_all(row_i,
      xpath = ".//*[(name()='th' or name()='td') and not(ancestor::th|ancestor::td)]"
    )
    for (cell in cells) {
      j <- j + 1
      rowspan <- xml2::xml_attr(cell, "rowspan")
      colspan <- xml2::xml_attr(cell, "colspan")
      rowspan <- ifelse(is.na(rowspan), 1, as.integer(rowspan))
      colspan <- ifelse(is.na(colspan), 1, as.integer(colspan))
      scratch <- grow_matrix(scratch, i - 1 + rowspan, j - 1 + colspan, FALSE)
      while (scratch[i, j]) {
        j <- j + 1
        scratch <- grow_matrix(scratch, i - 1 + rowspan, j - 1 + colspan, FALSE)
      }
      out[[i]][j] <- as.character(cell)
      scratch[
        seq(i, length.out = rowspan),
        seq(j, length.out = colspan)
      ] <- TRUE
    }
  }
  maxcols <- max(purrr::map_int(out, length))
  out <- purrr::map(
    out,
    function(.x) {
      length(.x) <- maxcols
      .x
    }
  )
  out <- purrr::transpose(out)
  out <- purrr::map(out, purrr::flatten_chr)
  out <- tibble::set_tidy_names(out, quiet = TRUE)
  out <- tibble::as_tibble(out, .name_repair = "minimal")
  out <- as_cells(out, row_names = FALSE, col_names = FALSE)
  out[, c("double", "integer", "logical")] <- NULL
  colnames(out) <- c("row", "col", "data_type", "html")
  out$data_type <- "html"
  dplyr::arrange(out, col, row)
}

as_cells.xml_document <- function(x, row_names = FALSE, col_names = FALSE) {
  tables <- xml2::xml_find_all(x, xpath = "//table[not(ancestor::table)]")
  lapply(tables, as_cells)
}
behead <- function(cells, direction, name, values = NULL, types = data_type,
                   formatters = list(), drop_na = TRUE) {
  UseMethod("behead")
}

behead.data.frame <- function(cells, direction, name, values = NULL,
                              types = data_type, formatters = list(),
                              drop_na = TRUE) {
  behead_if.data.frame(cells,
    direction = direction,
    name = !!rlang::ensym(name),
    values = !!rlang::enexpr(values),
    types = !!rlang::ensym(types),
    formatters = formatters, drop_na = drop_na
  )
}

behead_if <- function(cells, ..., direction, name, values = NULL, types =
                        data_type, formatters = list(), drop_na = TRUE) {
  UseMethod("behead_if")
}

behead_if.data.frame <- function(cells, ..., direction, name, values = NULL,
                                 types = data_type, formatters = list(),
                                 drop_na = TRUE) {
  dots <- rlang::enquos(...)
  check_direction_behead(direction)
  check_distinct(cells)
  name <- rlang::ensym(name)
  functions <- purrr::map(formatters, purrr::as_mapper)
  values <- rlang::enexpr(values)
  if (is.null(values)) {
    values_was_null <- TRUE
    types <- rlang::ensym(types)
  } else {
    values_was_null <- FALSE
    values <- rlang::ensym(values)
    types <- rlang::sym(".data_type")
    cells <-
      cells %>%
      dplyr::mutate(.value = !!values) %>%
      dplyr::mutate(!!types := ".value")
  }
  type_names <- unique(dplyr::pull(cells, !!types))
  filter_expr <- direction_filter(direction)
  is_header <- rlang::eval_tidy(filter_expr, cells)
  headers <-
    cells %>%
    dplyr::filter(is_header, !!!dots) %>%
    pack(types = !!types) %>%
    dplyr::mutate(
      is_na = is.na(value),
      !!name := purrr::imap(
        value,
        maybe_format_list_element,
        functions
      ),
      !!name := concatenate(!!name)
    ) %>%
    dplyr::filter(!(drop_na & is_na)) %>%
    dplyr::select(row, col, !!name)
  if (length(dots) == 0) {
    data_cells <- dplyr::filter(cells, !is_header)
  } else {
    data_cells <- dplyr::anti_join(cells, headers, by = c("row", "col"))
  }
  out <- enhead(data_cells, headers, direction, drop = FALSE)
  if (!values_was_null) out <- dplyr::select(out, -.value, -.data_type)
  out
}

direction_filter <- function(direction) {
  direction <- substr(direction, 1L, 1L)
  dplyr::case_when(
    direction == "N" ~ rlang::expr(.data$row == min(.data$row)),
    direction == "E" ~ rlang::expr(.data$col == max(.data$col)),
    direction == "S" ~ rlang::expr(.data$row == max(.data$row)),
    direction == "W" ~ rlang::expr(.data$col == min(.data$col))
  )
}

check_direction_behead <- function(direction_string) {
  directions <- c(
    "NNW", "N", "NNE",
    "ENE", "E", "ESE",
    "SSE", "S", "SSW",
    "WSW", "W", "WNW"
  )
  other_directions <- c("ABOVE", "LEFT", "RIGHT", "BELOW")
  if (direction_string %in% other_directions) {
    stop(
      "`direction` must be one of \"",
      paste(directions, collapse = "\", \""),
      "\".  To use the directions \"",
      paste(other_directions, collapse = "\", \""),
      "\" look at `?enhead`."
    )
  }
  if (!(direction_string %in% directions)) {
    stop(
      "`direction` must be one of \"",
      paste(directions, collapse = "\", \""),
      "\""
    )
  }
}
enhead <- function(data_cells, header_cells, direction, drop = TRUE) {
  UseMethod("enhead")
}

enhead.data.frame <- function(data_cells, header_cells, direction,
                              drop = TRUE) {
  check_header(header_cells)
  check_direction_enhead(direction)
  check_distinct(data_cells)
  check_distinct(header_cells)
  if (direction %in% c("ABOVE", "RIGHT", "BELOW", "LEFT")) {
    do.call(direction, list(data_cells, header_cells))
  } else if (direction %in% c(
    "N", "E", "S", "W",
    "NNW", "NNE",
    "ENE", "ESE",
    "SSE", "SSW",
    "WSW", "WNW"
  )) {
    do.call(direction, list(data_cells, header_cells, drop))
  }
}

N <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -row),
    by = "col",
    suffix = c(".data", ".header")
  )
  tibble::as_tibble(out)
}

E <- function(data_cells, header_cells, drop = TRUE) {
  check_header(header_cells)
  join <- ifelse(drop, dplyr::inner_join, dplyr::left_join)
  out <- join(data_cells, dplyr::select(header_cells, -col),
    by = "row",
    suffix = c(".data", ".header")
  )
  tibble::as_tibble(out)
}

S <- N
W <- E

NNW <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_left", drop)
}

NNE <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "top_right", drop)
}

SSE <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_right", drop)
}

SSW <- function(data_cells, header_cells, drop = TRUE) {
  corner_join(data_cells, header_cells, "bottom_left", drop)
}

WNW <- NNW
ENE <- NNE
ESE <- SSE
WSW <- SSW

corner_join <- function(data_cells, header_cells, corner, drop = TRUE) {
  check_header(header_cells)
  headers <-
    header_cells %>%
    partition(dplyr::distinct(header_cells, row, col),
      corner,
      nest = FALSE
    ) %>%
    dplyr::select(-row, -col)
  datas <- partition(data_cells,
    dplyr::distinct(header_cells, row, col),
    corner,
    nest = FALSE,
    strict = FALSE
  )
  out <-
    dplyr::inner_join(datas, headers,
      by = c("corner_row", "corner_col"),
      suffix = c("", ".y")
    ) %>%
    dplyr::select(-corner_row, -corner_col)
  if (!drop) {
    remainder <- dplyr::anti_join(data_cells, out, by = c("row", "col"))
    out <- dplyr::bind_rows(out, remainder)
  }
  out
}

ABOVE <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "NNW", drop)
}

LEFT <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "WNW", drop)
}

BELOW <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "SSW", drop)
}

RIGHT <- function(data_cells, header_cells, drop = TRUE) {
  side_join(data_cells, header_cells, "ENE", drop)
}

side_join <- function(data_cells, header_cells, corner, drop = TRUE) {
  check_header(header_cells)
  if (corner %in% c("NNW", "NNE", "SSW", "SSE")) {
    pos <- rlang::sym("col")
  } else {
    pos <- rlang::sym("row")
  }
  header_cells <- dplyr::mutate(
    header_cells,
    !!pos := corner_pos(!!pos, corner)
  )
  rlang::as_function(corner)(data_cells, header_cells, drop = drop)
}

corner_pos <- function(cells, corner) {
  corner_names <- c("NNW", "NNE", "ENE", "ESE", "SSE", "SSW", "WSW", "WNW")
  corner_poss <- rep(c("col", "col", "row", "row"), 2L)
  corner_looks <- c(
    rep(c(dplyr::lag, dplyr::lead), 2L),
    rep(c(dplyr::lead, dplyr::lag), 2L)
  )
  corner_defaults <- c(1L, 16384L, 1L, 1048576L, 16384L, 1L, 1048576L, 1L)
  corner_coefs <- c(2L, -2L, 2L, -2L, -2L, 2L, -2L, 2L)
  corner_extremes <- c(
    rep(c(floor, ceiling), 2L),
    rep(c(ceiling, floor), 2L)
  )
  corner_i <- match(corner, corner_names)
  pos <- rlang::sym(corner_poss[corner_i])
  look <- rlang::as_function(corner_looks[[corner_i]], ns_env("dplyr"))
  default <- corner_defaults[corner_i]
  extreme <- corner_extremes[[corner_i]]
  coef <- corner_coefs[corner_i]
  out <- extreme((cells + look(cells) + coef) / 2)
  out[is.na(out)] <- default
  out
}

check_header <- function(header_cells) {
  if (length(unique(header_cells$row)) > 1 & length(unique(header_cells$col)) > 1) {
    stop(
      "Multiple lines of headers are not supported in this way.",
      "\n  Perhaps you meant to concatenate them together first,",
      "\n  Or look at ?partition"
    )
  }
}

check_direction_enhead <- function(direction_string) {
  directions <- c(
    "NNW", "N", "NNE",
    "ENE", "E", "ESE",
    "SSE", "S", "SSW",
    "WSW", "W", "WNW",
    "ABOVE", "LEFT", "RIGHT", "BELOW"
  )
  if (!(direction_string %in% directions)) {
    stop(
      "`direction` must be one of \"",
      paste(directions, collapse = "\", \""),
      "\""
    )
  }
}

check_distinct <- function(cells) {
  if (dplyr::n_distinct(dplyr::select(cells, row, col)) != nrow(cells)) {
    stop("Row and column numbers must be distinct.",
      "\n  Perhaps you meant to use a single sheet.",
      call. = FALSE
    )
  }
}



fill_in_blanks <- function(sheet) {

  blank_df <-
    sheet %>%
    filter(data_type == "blank")

  joiner <- sheet %>% select(-character_formatted) %>% filter(!is_blank)

  inserter <-
    blank_df %>%
    mutate(col_old = col, col = col - 1) %>%
    mutate(address_old = address) %>%
    select(sheet, row, col, col_old, local_format_id, address_old) %>%
    left_join(joiner) %>%
    mutate(address = address_old) %>%
    select(-address_old) %>%
    mutate(col = col_old) %>%
    select(-col_old) %>%
    filter(!is_blank) %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    mutate(merged = 1)



  sheet <-
    sheet %>%
    mutate(row_col = paste0(row, "_", col)) %>%
    filter(!row_col %in% inserter$row_col) %>%
    bind_rows(inserter) %>%
    arrange(row, col)

  sheet %>% group_by(row, col) %>% top_n(n = 1, wt = row_number()) %>% ungroup()
}


get_col_groups <- function(sheet, value_ref, formats, ignore_bolding = FALSE, ignore_indenting = FALSE,  ignore_italics = FALSE) {
  
  
  header_df <-
    sheet %>%
    filter(!is_blank) %>%
    filter(col <= value_ref$max_col) %>%
    filter(col >= value_ref$min_col) %>%
    filter(row < value_ref$min_row) %>%
    mutate(row_temp = row)
  
  
  if(nrow(header_df) == 0){
    stop("No header groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  
  
  header_df <-
    header_df %>%
    mutate(bold = ifelse(ignore_bolding, NA,local_format_id %>% get_bolding_vec(sheet_format = formats))) %>%
    mutate(indent = ifelse(ignore_indenting, NA,local_format_id %>% get_indenting_vec(sheet_format = formats))) %>%
    mutate(italic = ifelse(ignore_italics, NA,local_format_id %>% get_italics_vec(sheet_format = formats)))
  
  
  header_df <-
    header_df %>%
    group_by(row_temp, indent, bold, italic) %>%
    mutate(merged = ifelse(sum(merged, na.rm = TRUE) == length(merged), T, F)) %>%
    filter(merged != T) %>%
    nest() %>%
    ungroup()
  
  header_df <-
    header_df %>%
    mutate(row_no_name = row_temp - min(row_temp) + 1) %>%
    mutate(header_label = paste0("header_label_", str_pad(row_number(), 2, side = "left", "0")))
  
  header_df <-
    header_df %>%
    mutate(data = map2(
      data, header_label,
      function(data, header_label) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)
        temp_df[[header_label]] <- temp_df$value
        temp_df %>% select(-value)
      }
    ))
  
  header_df <-
    header_df %>%
    mutate(direction = "N") %>%
    dplyr::select(header_label, direction, data, indent, bold, italic)
  

  header_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary) # %>%
}


get_meta_df <- function(sheet, value_ref, col_groups, formats) {

  meta_df <-
    sheet %>%
    filter(
      !is_blank,
      row <= max(col_groups$max_row),
      col < value_ref$min_col
    )


  meta_df <-
    meta_df %>%
    mutate(col_temp = col) %>%
    mutate(row_temp = row) %>%
    mutate(indent = local_format_id %>%
      map_int(possibly({
        ~ formats$local$alignment[["indent"]][[.x]]
      }, 0L)) %>%
      unlist()) %>%
    mutate(bold = local_format_id %>%
      map_lgl(possibly({
        ~ formats$local$font[["bold"]][[.x]]
      }, F)) %>%
      unlist()) %>%
    mutate(italic = local_format_id %>%
      map_lgl(possibly({
        ~ formats$local$font[["italic"]][[.x]]
      }, F)) %>%
      unlist())


  meta_df <-
    meta_df %>%
    group_by(col_temp, row_temp, indent, bold, italic) %>%
    nest() %>%
    ungroup() %>%
    mutate(col_no_name = col_temp - min(col_temp) + 1) %>%
    mutate(row_no_name = row_temp - min(row_temp) + 1) %>%
    mutate(header_name = paste0(
      "row_", str_pad(row_no_name, 2, "left", "0"),
      "_col_", str_pad(col_no_name, 2, "left", "0"),
      "_in", indent,
      "_b", as.integer(bold),
      "_it", as.integer(italic)
    )) %>%
    mutate(meta_data = paste0("meta_data_", str_pad(row_number(), 2, side = "left", "0"))) %>%
    mutate(data = map2(
      data, meta_data,
      function(data, meta_data) {
        temp_df <- data %>% select(row, col, character)
        temp_df[[meta_data]] <- temp_df$character
        temp_df %>% select(-character)
      }
    ))


  meta_df <-
    meta_df %>%
    mutate(direction = "WNW")

    meta_df %>%
    dplyr::select(meta_data, direction, data, indent, bold, italic) %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
}

get_orientation_df <- function(orientation){

  names(orientation) %>%
    get_range_dfs() %>%
    mutate(.orientation = orientation)

}


get_orientations_df <- function(orientations){

  o_length <- length(orientations)

  tibble(.header_group_num = c(1:o_length)) %>%
    mutate(data = .header_group_num %>%
             map({ ~ orientations[.x] %>% get_orientation_df})) %>%
    mutate(.orientation = data %>% map_chr(~.x[[1,".orientation"]] ) ) %>%
    mutate(.header_group = ifelse(str_detect(.orientation,"^N"), "col_label","row_label")) %>%
    group_by(.header_group) %>%
    mutate(.header_group_name = paste(.header_group,str_pad(row_number(),width = 2,side = "left",pad = "0" ), sep = "_" )) %>%
    ungroup %>%
    select(data, .header_group = .header_group_name) %>%
    unnest() 
}

get_range_df <- function(range){

  cell_ref_df <- as_tibble(cellranger::as.cell_limits(range))

  range_df <-
    cell_ref_df[,1:2] %>%
    set_names(c("min","max")) %>%
    mutate(dimension = c("row","col")) %>%
    gather(key, value, -dimension) %>%
    unite(label, key, dimension, sep = "_") %>%
    spread(label, value )

  expand.grid(row = c(range_df$min_row[1]:range_df$max_row[1]),
              col = c(range_df$min_col[1]:range_df$max_col[1]))

}


get_range_dfs <- function(range){

  range %>%
    str_split(",") %>%
    unlist %>%
    map(get_range_df) %>%
    bind_rows()
}



get_row_groups <- function(sheet, value_ref, col_groups, formats, added_row_groups, 
                           ignore_bolding = FALSE, ignore_indenting = FALSE,  ignore_italics = FALSE) {
  
  
  row_name_df <-
    sheet %>%
    filter(
      !is_blank,
      row <= value_ref$max_row,
      row > max(col_groups$max_row),
      col < value_ref$min_col
    )
  
  if(nrow(row_name_df) == 0){
    stop("No row groups have been detected. If you haven't already, try using the 'manual_value_references` argument")
  }
  
  
  row_name_df <-
    row_name_df %>%
    mutate(col_temp = col) %>%
    mutate(bold = ifelse(ignore_bolding, NA,local_format_id %>% get_bolding_vec(sheet_format = formats))) %>%
    mutate(indent = ifelse(ignore_indenting, NA,local_format_id %>% get_indenting_vec(sheet_format = formats))) %>%
    mutate(italic = ifelse(ignore_italics, NA,local_format_id %>% get_italics_vec(sheet_format = formats)))
  
  
  if (!is.null(added_row_groups)) {
    added_row_df <-
      tibble(address = added_row_groups) %>%
      mutate(added_group_no = row_number()) %>%
      unnest()
    
    row_name_df <-
      row_name_df %>%
      left_join(added_row_df) %>%
      mutate_at(
        .vars = vars(indent, bold, italic),
        .funs = funs(ifelse(is.na(added_group_no), ., NA))
      )
  } else {
    row_name_df <-
      row_name_df %>%
      mutate(added_group_no = NA)
  }
  
  row_name_df <-
    row_name_df %>%
    group_by(col_temp, indent, bold, italic, added_group_no) %>%
    nest() %>%
    ungroup()
  
  row_name_df <-
    row_name_df %>%
    arrange(col_temp, indent, italic, bold) %>%
    mutate(row_group = paste0("row_group_", str_pad(row_number(), 2, side = "left", "0")))
  
  row_name_df <-
    row_name_df %>%
    mutate(data = map2(
      data, row_group,
      function(data, row_group) {
        temp_df <- data %>%
          mutate(value = coalesce(
            as.character(numeric),
            as.character(character),
            as.character(logical),
            as.character(date)
          )) %>%
          select(row, col, value)
        
        temp_df[[row_group]] <- temp_df$value
        
        temp_df %>% select(-value)
      }
    ))
  
  row_name_df <-
    row_name_df %>%
    mutate(row_sum = map_dbl(data, ~ get_row_sum(data = .x, sheet = sheet)))
  
  row_name_df <-
    row_name_df %>%
    mutate(direction = ifelse(row_sum == 0, "WNW", "W")) %>%
    dplyr::select(row_group, direction, data, indent, bold, italic, added_group_no)



  row_name_df %>%
    mutate(data_summary = data %>%
      map(~ .x %>% summarise(
        min_col = min(col, na.rm = T), max_col = max(col, na.rm = T),
        min_row = min(row, na.rm = T), max_row = max(row, na.rm = T)
      ))) %>%
    unnest(data_summary)
}


get_row_sum <- function(data, sheet) {
  data %>%
    mutate(row_sum_values = map_dbl(
      row,
      function(x) {
        summarise(filter(sheet, row == x), filled = sum(numeric, na.rm = T))$filled
      }
    )) %>%
    summarise(row_sum_values = sum(row_sum_values, na.rm = T)) %>%
    pull(row_sum_values)
}




get_tabledata <- function(sheet, value_ref) {


  sheet %>%
    filter(
      !is_blank | !is.na(comment),
      row <= value_ref$max_row,
      row >= value_ref$min_row,
      col <= value_ref$max_col,
      col >= value_ref$min_col
    ) %>%
    filter(is.na(merged)) %>%
    mutate(value = coalesce(as.character(numeric), as.character(character), as.character(logical), as.character(date))) %>%
    select(row, col, value, comment)
}


get_value_references <- function(sheet, manual_value_references) {


  if (is.null(manual_value_references)) {
    sheet %>%
      filter(!is.na(numeric)) %>%
      summarise(
        min_row = min(row), max_row = max(row),
        min_col = min(col), max_col = max(col)
      )
  } else {

    cell_ref_df <- as_tibble(cellranger::as.cell_limits(manual_value_references))

    cell_ref_df[,1:2] %>%
      set_names(c("min","max")) %>%
      mutate(dimension = c("row","col")) %>%
      gather(key, value, -dimension) %>%
      unite(label, key, dimension, sep = "_") %>%
      spread(label, value )

  }
}
isolate_sentinels <- function(.data, col, sentinels, into = "sentinel") {
  col <- rlang::ensym(col)
  into <- rlang::ensym(into)
  col_class <- class(dplyr::pull(.data, !!col))[1]
  if (col_class %in% c("factor", "ordered", "list")) {
    stop(
      "Sentinels can't be isolated from a factors or lists (column `",
      rlang::expr_text(col),
      "`). Convert factors to character first, and choose elements of lists to turn into a vector."
    )
  }
  if (col_class != class(sentinels)[1]) {
    stop("The vector `sentinels` must be the same type (e.g. character, numeric) as `col`")
  }
  dplyr::mutate(
    .data,
    !!into := dplyr::if_else(
      !!col %in% sentinels,
      !!col,
      na_of_type(!!col)
    ),
    !!col := dplyr::if_else(is.na(!!into), !!col, na_of_type(!!col))
  )
}
justify <- function(header_cells, corner_cells) {
  UseMethod("justify")
}

justify.data.frame <- function(header_cells, corner_cells) {
  stopifnot(nrow(header_cells) == nrow(corner_cells))
  header_cells <- dplyr::arrange(header_cells, row, col)
  corner_cells <- dplyr::arrange(corner_cells, row, col)
  header_cells$row <- corner_cells$row
  header_cells$col <- corner_cells$col
  header_cells
}
NULL

merge_rows <- function(cells, rows, values, collapse = " ") {
  UseMethod("merge_rows")
}

merge_rows.data.frame <- function(cells, rows, values, collapse = " ") {
  values <- rlang::ensym(values)
  non_values <- setdiff(
    names(cells),
    c(rlang::expr_text(rlang::enexpr(values)), "col")
  )
  summaries <- purrr::map(
    paste0("dplyr::first(", non_values, ")"),
    rlang::parse_expr
  )
  names(summaries) <- non_values
  merged_cells <-
    dplyr::filter(cells, row %in% rows) %>%
    dplyr::mutate(row = min(row)) %>%
    dplyr::arrange(col, row) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(
      !!!summaries,
      !!values := paste(!!values, collapse = collapse)
    )
  other_cells <-
    cells %>%
    dplyr::filter(!(row %in% rows))
  dplyr::bind_rows(merged_cells, other_cells)
}

merge_cols <- function(cells, cols, values, collapse = " ") {
  UseMethod("merge_cols")
}

merge_cols.data.frame <- function(cells, cols, values, collapse = " ") {
  values <- rlang::ensym(values)
  non_values <- setdiff(
    names(cells),
    c(rlang::expr_text(rlang::enexpr(values)), "row")
  )
  summaries <- purrr::map(
    paste0("dplyr::first(", non_values, ")"),
    rlang::parse_expr
  )
  names(summaries) <- non_values
  merged_cells <-
    dplyr::filter(cells, col %in% cols) %>%
    dplyr::mutate(col = min(col)) %>%
    dplyr::arrange(row, col) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(
      !!!summaries,
      !!values := paste(!!values, collapse = collapse)
    )
  other_cells <-
    cells %>%
    dplyr::filter(!(col %in% cols))
  dplyr::bind_rows(merged_cells, other_cells)
}

migrate <- function(orientated_df){
  
  orientated_df_nested <-
    orientated_df %>%
    filter(!is.na(.orientation)) %>% 
    group_by(.orientation, .header_group) %>%
    mutate(value = coalesce(character,as.character(numeric))) %>%
    select(row,col,.value,.orientation,.header_group) %>%
    nest()
  
  data_row_index <- which(orientated_df_nested$.orientation == "data")
  header_dfs   <- orientated_df_nested$data[orientated_df_nested$.orientation != "data"]
  orientations <- orientated_df_nested$.orientation[orientated_df_nested$.orientation != "data"]
  header_names <- orientated_df_nested$.header_group[orientated_df_nested$.orientation != "data"]
  
  header_dfs <- 
    map2(header_dfs,header_names, function(header_df, header_name){
      header_df %>% 
        rename(!!sym(header_name) := .value)
    })
  
  list(
    x = header_dfs,
    y = orientations) %>%
    pmap(function(x,y){
      enhead_tabledata(header_data = x, direction = y,
                       values = orientated_df_nested$data[[data_row_index]])} ) %>%
    reduce(full_join,by = c("row", "col",".value"))  
  

  
}




enhead_tabledata <- function(header_data, direction, values = tabledata) {
  unpivotr::enhead(
    data_cells = values,
    header_cells = header_data,
    direction = direction) 
}



orientate <- function(tidyxl_df, orientations){

  orientations_df <-
    orientations %>%
    get_orientations_df() 
  
  tidyxl_df %>%
    left_join(orientations_df, by = c("row", "col")) %>% 
    mutate(.value = coalesce(
      as.character(numeric),
      as.character(character),
      as.character(logical),
      as.character(date)
    )) 
  
}


orientate_auto <-
  function(sheet,formats, manual_value_references = NULL, added_row_groups = NULL, keep_meta_data = FALSE) {

      if (!is.null(manual_value_references)) {
        if (!tidyxl::is_range(manual_value_references)) {
          stop("manual_value_references must be a range")
        }
      }


    continue <- TRUE

    while (continue) {
      sheet_original <- sheet
      sheet <- sheet %>% fill_in_blanks()

      continue <- !identical(sheet_original, sheet)
    }

    manual_value_references_temp <- manual_value_references
    value_ref <- sheet %>% get_value_references(manual_value_references = manual_value_references_temp)

    added_row_groups_temp <- added_row_groups
    col_groups <- get_col_groups(sheet = sheet, value_ref = value_ref, formats = formats)
    row_groups <- get_row_groups(
      sheet = sheet, value_ref = value_ref,
      formats = formats, col_groups = col_groups, added_row_groups = added_row_groups_temp
    )


    meta_df <- get_meta_df(sheet = sheet, value_ref = value_ref, formats = formats, col_groups = col_groups)
    tabledata <- get_tabledata(sheet = sheet, value_ref = value_ref)

    unique_cols <- col_groups$data %>% map(3) %>% map(unique)
    unique_meta <- meta_df$data %>% map(3) %>% map(unique)
    unique_rows <- row_groups$data %>% map(3) %>% map(unique)

    if(length(unique_meta) > 0){
      joint_col <-
        full_join(
          tibble(values = unique_cols) %>%
            mutate(col_group = row_number()) %>%
            unnest(),
          tibble(values = unique_meta) %>%
            mutate(meta_group = row_number()) %>%
            unnest()
        )

      cols_to_keep <-
        joint_col %>%
        group_by(col_group) %>%
        filter(!is.na(col_group)) %>%
        summarise(keep = 1 != sum(!is.na(meta_group) / length(meta_group), na.rm = TRUE)) %>%
        pull(keep)


      col_groups <- col_groups[cols_to_keep, ]

    }


    col_groups <- col_groups %>% rename(.orientation = direction, .header_group = col_group) %>% select(.header_group,.orientation,data) %>% unnest()
    vars_to_combine <- names(col_groups)[!names(col_groups) %in% c(".header_group",".orientation","col","row")]
    col_groups <- col_groups %>% mutate(value = coalesce(!!!syms(vars_to_combine))) %>% select(-vars_to_combine)

    row_groups <- row_groups %>% rename(.orientation = direction, .header_group = row_group) %>% select(.header_group,.orientation,data) %>% unnest()
    vars_to_combine <- names(row_groups)[!names(row_groups) %in% c(".header_group",".orientation","col","row")]
    row_groups <- row_groups %>% mutate(value = coalesce(!!!syms(vars_to_combine))) %>% select(-vars_to_combine)

    tabledata <- tabledata %>% mutate(.header_group = "data", .orientation = "data")

    df_list_to_row_bind <- list()

    if(length(col_groups) > 0){df_list_to_row_bind <- df_list_to_row_bind %>% append(list(col_groups))}
    if(length(row_groups) > 0){df_list_to_row_bind <- df_list_to_row_bind %>% append(list(row_groups))}

    to_join <- df_list_to_row_bind  %>% append(list(tabledata)) %>% bind_rows() %>% select(-comment) %>% rename(.value = value)

    sheet %>% left_join(to_join)


      }
pack <- function(cells, types = data_type, name = "value", drop_types = TRUE,
                 drop_type_cols = TRUE) {
  types <- rlang::ensym(types)
  name <- rlang::ensym(name)
  type_colnames <- format(unique(dplyr::pull(cells, !!types)),
    justify = "none",
    trim = TRUE
  )
  missing_types <- setdiff(type_colnames, colnames(cells))
  new_cols <- rep_len(NA, length(missing_types))
  names(new_cols) <- missing_types
  cells <- dplyr::mutate(
    cells,
    !!!new_cols,
    !!types := format(!!types,
      justify = "none",
      trim = TRUE
    )
  )
  type_values <- unique(dplyr::pull(cells, !!types))
  patterns <-
    map(
      type_values,
      ~ rlang::expr(!!types == !!.x ~ as.list(!!rlang::ensym(.x)))
    )
  out <- dplyr::mutate(cells, !!name := dplyr::case_when(!!!patterns))
  names(out[[rlang::expr_text(name)]]) <- dplyr::pull(cells, !!types)
  if (drop_types && rlang::expr_text(types) != rlang::expr_text(name)) {
    out <- dplyr::select(out, -!!types)
  }
  if (drop_type_cols) {
    type_colnames <- setdiff(type_colnames, rlang::expr_text(name))
    out <- dplyr::select(out, -dplyr::one_of(type_colnames))
  }
  out
}

unpack <- function(cells, values = value, name = "data_type",
                   drop_packed = TRUE) {
  values <- rlang::ensym(values)
  name <- rlang::ensym(name)
  types <- names(dplyr::pull(cells, !!values))
  type_names <- format(unique(types), justify = "none", trim = TRUE)
  assignments <- purrr::map(
    type_names,
    ~ rlang::expr(ifelse(types == !!.x,
      !!values,
      !!list(NULL)
    ))
  )
  names(assignments) <- type_names
  out <-
    dplyr::mutate(cells, !!name := types, !!!assignments) %>%
    dplyr::mutate_at(type_names,
      concatenate,
      combine_factors = FALSE,
      fill_factor_na = FALSE
    )
  first_colnames <- setdiff(colnames(out), type_names)
  last_colnames <- sort(type_names)
  out <- dplyr::select(out, first_colnames, last_colnames)
  if (drop_packed) out <- dplyr::select(out, -!!values)
  out
}
partition <- function(cells, corners, align = "top_left",
                      nest = TRUE, strict = TRUE) {
  check_distinct(cells)
  if (!(align %in% c(
    "top_left", "top_right",
    "bottom_left", "bottom_right"
  ))) {
    stop("`align` must be one of:
         \"top_left\", \"top_right\", \"bottom_left\", \"bottom_right\"")
  }
  row_bound <- (if (align %in% c("top_left", "top_right")) "upper" else "lower")
  col_bound <- (if (align %in% c("top_left", "bottom_left")) "upper" else "lower")
  row_groups <- partition_dim(cells$row, sort(unique(corners$row)), row_bound)
  col_groups <- partition_dim(cells$col, sort(unique(corners$col)), col_bound)
  join_names <- c("row", "col")
  names(join_names) <- c("corner_row", "corner_col")
  out <-
    cells %>%
    dplyr::mutate(
      corner_row = row_groups,
      corner_col = col_groups
    ) %>%
    tidyr::nest(-corner_row, -corner_col, .key = "cells") %>%
    dplyr::inner_join(corners, by = join_names)
  if (strict) {
    out <- dplyr::filter(out, purrr::map_lgl(cells, contains_corner, corners))
  }
  if (!nest) {
    out <- tidyr::unnest(out)
  }
  out
}

contains_corner <- function(.data, corners) {
  nrow(dplyr::inner_join(.data, corners, by = c("row", "col"))) != 0L
}

partition_dim <- function(positions, cutpoints, bound = "upper") {
  if (!(bound %in% c("upper", "lower"))) {
    stop("`bound` must be one of \"upper\", \"lower\"")
  }
  cutpoints <- sort(c(-Inf, cutpoints, Inf), decreasing = bound == "lower")
  labels <- trunc(sort(cutpoints[-length(cutpoints)]))
  labels[is.infinite(labels)] <- NA
  out <- cut(positions, cutpoints, right = bound == "lower", labels = FALSE)
  labels[out]
}




plot_orientations <- function(orientated_df){
  orientated_df %>% 
    ggplot(aes(
      x = col, y = -row, fill = str_to_title(str_replace_all(.header_group, "_", " ")),
      label = ifelse(.header_group != "data", paste(
        str_extract(.header_group, "[0-9]{1,2}"),
        paste0(ifelse(.header_group != "data", paste0("(", .orientation, ")"), ""))
      ), "")
    )) +
    geom_tile() +
    geom_text(size = 3) +
    theme_minimal() +
    labs(fill = "Cell Type", y = "Row", x = "Column")
}
"purpose"
rectify <- function(cells, values = NULL, types = data_type,
                    formatters = list()) {
  UseMethod("rectify")
}

rectify.data.frame <- function(cells, values = NULL, types = data_type,
                               formatters = list()) {
  check_distinct(cells)
  if (nrow(cells) == 0L) return(tibble::tibble())
  values <- rlang::enexpr(values)
  types <- rlang::ensym(types)
  cells <- pad(cells)
  if (is.null(values)) {
    out <-
      cells %>%
      pack() %>%
      dplyr::select(row, col, value) %>%
      unpack() %>%
      spatter(col, types = data_type, formatters = formatters)
  } else {
    cells <- dplyr::select(cells, row, col, !!values)
    if (rlang::expr_text(values) == "row") {
      cells$.row <- cells$row
      values <- rlang::sym(".row")
    }
    out <- spatter(cells, col, values = !!values, types = !!types)
  }
  minrow <- min(cells$row)
  mincol <- min(cells$col)
  nrows <- max(cells$row) - minrow + 1L
  ncols <- max(cells$col) - mincol + 1L
  colnums <- seq_len(ncols) + mincol - 1L
  colnums <- paste0(colnums, "(", cellranger::num_to_letter(colnums), ")")
  rownums <- seq_len(nrows) + minrow - 1L
  colnames(out) <- c("row/col", colnums)
  out$`row/col` <- rownums
  class(out) <- c("cell_grid", class(out))
  out
}

print.cell_grid <- function(x, display = "terminal", ...) { # nocov start
  if (display == "terminal") {
    NextMethod(x)
  } else if (display == "browser") {
    if (!("DT" %in% installed.packages())) {
      "You need to install the 'DT' package to view this in the browser."
    }
    DT::datatable(x,
      extensions = c(
        "FixedHeader",
        "KeyTable",
        "Scroller"
      ),
      options = list(
        paging = FALSE,
        fixedHeader = TRUE,
        keys = TRUE
      ),
      rownames = FALSE
    )
  } else if (display == "rstudio") {
    View(x)
  } else {
    NextMethod(x)
  }
} # nocov end

pad <- function(cells, rows = cells$row, cols = cells$col) {
  padding <- tidyr::crossing(
    row = tidyr::full_seq(c(cells$row, rows), 1L),
    col = tidyr::full_seq(c(cells$col, cols), 1L)
  )
  dplyr::full_join(padding, cells, by = c("row", "col"))
}
spatter <- function(cells, key, values = NULL, types = data_type,
                    formatters = list()) {
  UseMethod("spatter")
}

spatter.data.frame <- function(cells, key, values = NULL, types = data_type,
                               formatters = list()) {
  key <- rlang::ensym(key)
  functions <- purrr::map(formatters, purrr::as_mapper)
  values <- rlang::enexpr(values)
  new_colnames <- format(sort(unique(dplyr::pull(cells, !!key))),
    justify = "none",
    trim = TRUE
  )
  if (is.null(values)) {
    types <- rlang::ensym(types)
    original_types <- NULL
  } else {
    original_types <- rlang::ensym(types)
    types <- rlang::sym(".data_type")
    cells <-
      cells %>%
      dplyr::mutate(.value = !!values) %>%
      dplyr::mutate(!!types := ".value")
    if (!(rlang::expr_text(values) %in% c(rlang::expr_text(key)))) {
      cells <- dplyr::select(cells, -!!values)
    }
  }
  if (is.null(values)) {
    drop_types <- rlang::expr_text(types) != rlang::expr_text(key)
  } else {
    drop_types <- !(c(".data_type") %in% c(
      rlang::expr_text(key),
      rlang::expr_text(values)
    ))
  }
  out <- pack(cells, types = !!types, name = ".value", drop_types = drop_types)
  n_keys <- length(unique(dplyr::pull(cells, !!key)))
  n_cols <- ncol(out) - 2 + n_keys
  new_col_positions <- seq_len(n_keys) + (n_cols - n_keys)
  out <-
    out %>%
    dplyr::mutate(.value = purrr::imap(
      .value,
      maybe_format_list_element,
      functions
    )) %>%
    tidyr::spread(!!key, .value) %>%
    dplyr::mutate_at(new_col_positions, concatenate)
  if (!is.null(original_types) &&
    rlang::expr_text(original_types) %in% colnames(out)) {
    out <- dplyr::select(out, -!!original_types)
  }
  first_colnames <- setdiff(colnames(out), new_colnames)
  last_colnames <- new_colnames
  out <- dplyr::select(out, first_colnames, last_colnames)
  out
}
tidy_table <- function(x, row_names = FALSE, col_names = FALSE) {
  message("tidy_table() will be deprecated.  Use as_cells() instead.")
  UseMethod("as_cells")
}
"_PACKAGE"


unpivotr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "unpivotr")) %>% .[str_detect(.,"xlsx$")]
  } else {
    system.file("extdata", path, package = "unpivotr", mustWork = TRUE) %>% .[str_detect(.,"xlsx$")]
  }
}





magrittr::`%>%`




NULL

globalVariables(c(
  ".",
  "inner_join",
  "mutate",
  "select",
  "rename",
  "quo",
  "UQ",
  "quo_name",
  "from_row",
  "from_col",
  "to_row",
  "to_col",
  "type",
  "value",
  "everything",
  "data_type",
  "is_na",
  ".value",
  ".data_type",
  "n",
  ":=",
  ".partition",
  "ns_env",
  "corner_row",
  "corner_col",
  ".boundary"
))

concatenate <- function(..., combine_factors = TRUE, fill_factor_na = TRUE) {
  c.POSIXct <- function(..., recursive = FALSE) {
    .POSIXct(c(unlist(lapply(list(...), unclass))), tz = "UTC")
  }
  dots <- (...)
  dots_is_null <- purrr::map_lgl(dots, rlang::is_null)
  if (all(dots_is_null)) {
    return(dots)
  }
  dots_is_scalar_vector <- purrr::map_lgl(dots, rlang::is_scalar_vector)
  if (any(!dots_is_scalar_vector[!dots_is_null])) {
    return(dots)
  }
  classes <- purrr::map(dots, class)
  if (length(unique(classes[!dots_is_null])) == 1L) {
    all_classes <- classes[!dots_is_null][[1]]
    first_class <- all_classes[1]
    if (first_class %in% c("factor", "ordered")) {
      if (combine_factors || fill_factor_na) {
        dots[dots_is_null] <- list(factor(NA_character_))
      }
      if (combine_factors) {
        return(forcats::fct_c(rlang::splice(dots)))
      }
      else {
        return(dots)
      }
    } else {
      dots[dots_is_null] <- NA
      dots <- do.call(c, dots)
      class(dots) <- all_classes
      return(dots)
    }
  }
  dots[dots_is_null] <- NA
  factors <- purrr::map_lgl(classes, ~ .[1] %in% c("factor", "ordered"))
  dots[factors] <- purrr::map(dots[factors], as.character)
  dates <- purrr::map_lgl(classes, ~ .[1] %in% c("Date", "POSIXct", "POSIXlt"))
  dots[dates] <- purrr::map(dots[dates], format, justify = "none", trim = TRUE)
  do.call(c, dots)
}

na_types <- list(
  logical = NA,
  integer = NA_integer_,
  double = NA_real_,
  character = NA_character_,
  complex = NA_complex_
)
na_of_type <- function(x) structure(na_types[[typeof(x)]], class = class(x))

maybe_format_list_element <- function(x, name, functions) {
  func <- functions[[name]]
  if (is.null(func)) func <- identity
  func(x)
}
