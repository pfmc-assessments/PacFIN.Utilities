#' Write SQL Text for Catches
#'
#' Write SQL text as a single character string that will result in
#' getting the catch data from the PacFIN database.
#'
#' @template pacfin_species_code
#' @template returnsql
#' @author Kelli F. Johnson
#'
sql.catch <- function(pacfin_species_code) {
  spid <- paste0("('", paste(pacfin_species_code, collapse = "','"), "')")
  sqlcall <- paste0(
  "Select *
   from pacfin_marts.Comprehensive_FT
   where PACFIN_SPECIES_CODE = any ", spid, " 
          and COUNCIL_CODE = 'P'")
  sqlcall <- gsub("\\n", "", sqlcall)
   return(sqlcall)
}
