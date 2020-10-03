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
          COUNCIL_CODE COUNCIL,
          AGENCY_CODE,
          AGENCY_CODE AGID,
          DAHL_GROUNDFISH_CODE,
          DAHL_GROUNDFISH_CODE DAHL_SECTOR,
          INPFC_AREA_TYPE_CODE,
          INPFC_AREA_TYPE_CODE INPFC_ARID,
          PACFIN_CATCH_AREA_CODE,
          PACFIN_CATCH_AREA_CODE ARID,
          LANDING_YEAR,
          LANDING_YEAR YEAR,
          LANDING_MONTH PERIOD,
          LANDING_DATE,
          LANDING_DATE TDATE,
          FTID,
          PARTICIPATION_GROUP_CODE,
          PARTICIPATION_GROUP_CODE PARGRP,
          ORIG_PACFIN_CATCH_AREA_CODE,
          PACFIN_PORT_CODE,
          PACFIN_PORT_CODE PCID,
          FLEET_CODE,
          FLEET_CODE FLEET,
          VESSEL_ID,
          PACFIN_GEAR_CODE,
          PACFIN_GEAR_CODE GRID,
          IS_IFQ_LANDING,
          IS_IFQ_LANDING IFQ_LANDING,
          REMOVAL_TYPE_CODE,
          REMOVAL_TYPE_CODE REMOVAL_TYPE,
          CONDITION_CODE,
          CONDITION_CODE COND,
          DISPOSITION_CODE,
          DISPOSITION_CODE DISP,
          EXVESSEL_REVENUE,
          EXVESSEL_REVENUE REV,
          PACFIN_SPECIES_CODE,
          PACFIN_SPECIES_CODE SPID,
          NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE,
          IS_SPECIES_COMP_USED,
          GRADE_CODE,
          GRADE_CODE GRADE,
          GRADE_NAME,
          PACFIN_GROUP_GEAR_CODE,
          PACFIN_GROUP_GEAR_CODE GRGROUP,
          ROUND_WEIGHT_LBS,
          ROUND_WEIGHT_LBS CATCH_LBS,
          ROUND_WEIGHT_MTONS
   from pacfin_marts.Comprehensive_FT
   where PACFIN_SPECIES_CODE = any ", spid, " 
          and COUNCIL_CODE = 'P'
          and AGENCY_CODE in ('W','O','C')")
  sqlcall <- gsub("\\n", "", sqlcall)
   return(sqlcall)
}
