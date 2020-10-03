#' Write SQL Text for Species
#'
#' Write SQL text as a single character string that will result in
#' getting the species data from the PacFIN database.
#'
#' @template returnsql
#' @author Kelli Faye Johnson
#'
sql.species <- function() {
  sqlcall <- "SELECT
      distinct PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME
    FROM PACFIN_MARTS.COMPREHENSIVE_FT
    ORDER BY PACFIN_SPECIES_COMMON_NAME, PACFIN_SPECIES_CODE"
  return(sqlcall)
}
