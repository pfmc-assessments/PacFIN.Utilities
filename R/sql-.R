#' Write SQL text
#'
#' Write SQL text as a single character string that will result in getting the
#' relevant data from the PacFIN database.
#'
#' @param pacfin_species_code A vector of strings specifying the PacFIN species
#'   code(s) you are interested in. This has sometimes been referred to as
#'   `"SPID"` in legacy sql scripts. An example for sablefish would be
#'   `pacfin_species_code = "SABL"`. Lists of species codes in [hierarchical
#'   order](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp_tree.txt),
#'   by
#'   [organization](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_spcodes.txt),
#'   and [alphabetically
#'   organized](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_spcodes_spid.txt)
#'   can be found on the [PacFIN website](https://pacfin.psmfc.org). Often you
#'   will want to include nominal species categories. Where, nominal (i.e.,
#'   [existing in name
#'   only](https://www.google.com/search?q=NOMINAL&rlz=1C1GCEJ_enUS866US866&sourceid=chrome&ie=UTF-8))
#'   means information for a given species that is "derived" from non-species
#'   specific information, e.g., species complexes that are split out by species
#'   compositions like "nominal aurora rockfish" which would be `ARR1`. For some
#'   functions, these nominal categories can automatically be added, see the
#'   argument `addnominal`.
#' @param council_code A vector of character strings specifying the council code
#'   that you wish to retain data for. The default `"P"` means data will only be
#'   returned for the Pacific Fisheries Management Council. Other accepted
#'   values are `"N"` or `"*"`.
#'
#' @return A single character string formatted as an sql call.
#' @author Kelli F. Johnson and John R. Wallace
#' @name sql
NULL
#'
#' @rdname sql
#' @details `sql_area()` results in area data
sql_area <- function() {
  sqlcall <- glue::glue("
    SELECT *
    FROM PACFIN.BDS_AR;
    ")
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
#'
#' @rdname sql
#' @details `sql_bds()` results in biological data
sql_bds <- function(pacfin_species_code) {
  # todo: think about changing
  # * VESSEL_NUM to VESSEL_ID b/c less NULL
  # * TOTAL_WGT, might be spp weight
  # * WGTMAX, i don't think this one matters
  # * WGTMIN, i don't think this one matters
  # * all_cluster_sum, done with ave

  # Use a regular expression rather than any because regex allows for
  # spaces before and after
  spid <- sQuote(paste(pacfin_species_code, collapse = "|"), q = FALSE)
  stopifnot(length(spid) == 1)

  sqlcall <- glue::glue("
   SELECT *
   FROM PACFIN_MARTS.COMPREHENSIVE_BDS_COMM
   WHERE REGEXP_LIKE (PACFIN_SPECIES_CODE, {spid});
   ")
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
#'
#' @rdname sql
#' @details `sql_bds()` results in catch data
sql_catch <- function(pacfin_species_code, council_code = "P") {
  species <- paste(sQuote(pacfin_species_code, q = FALSE), collapse = ", ")
  stopifnot(length(species) == 1)
  council <- paste(
    sQuote(
      match.arg(council_code, choices = c("P", "N", "*"), several.ok = TRUE),
      q = FALSE
    ),
    collapse = ", "
  )
  stopifnot(length(council) == 1)

  sqlcall <- glue::glue("
    SELECT *
    FROM PACFIN_MARTS.COMPREHENSIVE_FT
    WHERE PACFIN_SPECIES_CODE = ANY ({species})
     AND COUNCIL_CODE = ANY ({council})
    ")
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
#'
#' @rdname sql
#' @details `sql_species()` results in data frame of species names
sql_species <- function() {
  sqlcall <- glue::glue("
    SELECT DISTINCT PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME
    FROM PACFIN_MARTS.COMPREHENSIVE_FT
    ORDER BY PACFIN_SPECIES_COMMON_NAME, PACFIN_SPECIES_CODE;
    ")
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
#'
#' @rdname sql
#' @details
#' `sql_check_FINAL_FISH_AGE_IN_YEARS()` results in a data frame of number of
#' unique `FISH_ID`s present in PacFIN per `PACFIN_SPECIES_CODE` and
#' `AGENCY_CODE`, i.e., species and state sampling agency, that have entries for
#' `AGE_IN_YEARS` but have `NULL` values for `FINAL_FISH_AGE_IN_YEARS`. In
#' theory, this table should have zero rows. But, there may be reasons why these
#' entries do not have a final age value and potentially should not be used in
#' assessments of population status. Thus, the result is used in an automated
#' check and to create warning labels when downloading data.
sql_check_FINAL_FISH_AGE_IN_YEARS <- function() {
  sqlcall <- glue::glue("
    SELECT PACFIN_SPECIES_CODE, AGENCY_CODE, count(distinct(FISH_ID)) AS N
    FROM PACFIN_MARTS.COMPREHENSIVE_BDS_COMM
    WHERE AGE_IN_YEARS is not null and FINAL_FISH_AGE_IN_YEARS is null
    group by PACFIN_SPECIES_CODE, AGENCY_CODE
    ORDER by PACFIN_SPECIES_CODE, AGENCY_CODE;
    ")
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
