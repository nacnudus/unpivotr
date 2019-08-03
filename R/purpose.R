#' Sense-of-purpose in the 2014 New Zealand General Social Survey
#'
#' A dataset containing the self-rated sense-of-purpose of respondents to the
#' 2014 New Zealand General Social Survey.
#'
#' @format A list of eight data frames.  The first data frame, `Tidy`, contains
#' the raw data in a standard tabular format:
#'
#' * `Sex` Character, two levels
#' * `Age group (Life-stages)` Character, age-range in years, four levels
#' * `Highest qualification` Character, five levels
#' * `Sense of purpose` Character, score-range, two levels and NA
#' * `Value` Numeric, number of respondents (weighted? rounded?), has NAs
#' * `Flags` Character, metadata flags, two levels and NA
#'
#' The next six data frames are pivot tables of the first data frame.  The
#' data frames are named by the compass directions that are suggested for
#' unpivoting them.
#'
#' The final data frame is a 'tidy' representation of small-multiple pivot
#' tables.
#'
#' @source
#' The data is 'Sense of purpose by highest qualification, age group, and sex,
#' 2014' from the Statistics New Zealand portal NZ.Stat
#' <http://nzdotstat.stats.govt.nz/wbos/Index.aspx#>, retrieved on
#' 2016-08-19.  It can be found in the section 'People and communities' >
#' 'Self-rated well-being (NZGSS)'.  The data was exported in the Excel (.xlsx)
#' file format and is available at 'extdata/purpose.xlsx' in the package
#' directory.
#'
#' @details
#' The description provided by Statistics New Zealand is below.
#'
#' "The 2014 New Zealand General Social Survey (NZGSS) is the fourth of the
#' survey series. We run the NZGSS every two years and interview around 8,500
#' people about a range of social and economic outcomes.
#'
#' It provides new and redeveloped data about different aspects of people's
#' lives and their well-being. In particular, the survey provides a view of how
#' well-being outcomes are distributed across different groups within the New
#' Zealand population.
#'
#' Symbols used in this table:
#'
#' * `S` Data has been suppressed.
#' * `*` Relative sampling error of 50 percent or more. Numbers may not add to
#' the total because 'Don't know' and 'Refused' have been excluded.
#'
#' For more tables using the NZGSS 2014 first release see
#' <http://archive.stats.govt.nz/browse_for_stats/people_and_communities/Well-being/nzgss-info-releases.aspx>.
#'
#' Data quality:
#' These statistics have been produced in accordance with the Official
#' Statistics System principles and protocols for quality. They conform to the
#' Statistics NZ Methodological Standard for Reporting of Data Quality."
"purpose"
