#' get tidyABS components
#'
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
      # Remove meta data /col group duplicates
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
