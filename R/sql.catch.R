#' Write SQL Text for Catches
#'
#' Write SQL text as a single character string that will result in
#' getting the catch data from the PacFIN database.
#'
#' @template pacfin_species_code
#' @template returnsql
#' @author John R. Wallace, Kelli Faye Johnson
#'
sql.catch <- function(pacfin_species_code) {
  spid <- paste0("('", paste(pacfin_species_code, collapse = "','"), "')")
  sqlcall <- paste0(
  "Select COUNCIL_CODE,
          AGENCY_CODE,
          DAHL_GROUNDFISH_CODE,
          INPFC_AREA_TYPE_CODE,
          PACFIN_CATCH_AREA_CODE,
          LANDING_YEAR,
          LANDING_MONTH,
          LANDING_DATE,
          FTID,
          PARTICIPATION_GROUP_CODE,
          ORIG_PACFIN_CATCH_AREA_CODE,
          PACFIN_PORT_CODE,
          FLEET_CODE,
          VESSEL_ID,
          PACFIN_GEAR_CODE,
          IS_IFQ_LANDING,
          REMOVAL_TYPE_CODE,
          CONDITION_CODE,
          DISPOSITION_CODE,
          EXVESSEL_REVENUE,
          PACFIN_SPECIES_CODE,
          NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE,
          IS_SPECIES_COMP_USED,
          GRADE_CODE,
          GRADE_NAME,
          PACFIN_GROUP_GEAR_CODE,
          ROUND_WEIGHT_LBS,
          ROUND_WEIGHT_MTONS
   from pacfin_marts.Comprehensive_FT
   where PACFIN_SPECIES_CODE = any ", spid, " 
          and COUNCIL_CODE = 'P'
          and AGENCY_CODE in ('W','O','C')")
  sqlcall <- gsub("\\n", "", sqlcall)
   return(sqlcall)
}
