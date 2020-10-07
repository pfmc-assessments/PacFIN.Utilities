#' Change Column Names to Vdrfd
#'
#' Change column names of comprehensive PacFIN columns to those
#' used in legacy code.
#'
#' @param data A data frame with column names.
#' @param use Specify the column name category you would like to switch to.
#' \code{match.arg} is used to allow for partial matches.
#' Options include
#' `r paste0("``", eval(formals(cleanColumns)[["use"]]), "``", collapse = ", ")`.
#' \code{"vdrfd"} will return the column names used prior to 2020, the default.
#' \code{"raw"} will return the original column names as downloaded.
#'
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return An augmented data frame with new column names.
#'
cleanColumns <- function(data, use = c("vdrfd", "raw")) {
  use <- match.arg(use)
  master <- matrix(scan(text = "
    Comp_FT                      vdrfd
    COUNCIL_CODE                 COUNCIL
    AGENCY_CODE                  AGID
    LANDING_YEAR                 YEAR
    LANDING_DATE                 TDATE
    PACFIN_SPECIES_CODE          SPID
    PARTICIPATION_GROUP_CODE     PARGRP
    PACFIN_PORT_CODE             PCID
    PACFIN_CATCH_AREA_CODE       ARID
    INPFC_AREA_TYPE_CODE         INPFC_ARID
    PORT_CODE                    PORT
    FLEET_CODE                   FLEET
    VESSEL_NUM                   DRVID
    PACFIN_GEAR_CODE             GRID
    PACFIN_GROUP_GEAR_CODE       GRGROUP
    IS_IFQ_LANDING               IFQ_LANDING
    REMOVAL_TYPE_CODE            REMOVAL_TYPE
    CONDITION_CODE               COND
    DISPOSITION_CODE             DISP
    EXVESSEL_REVENUE             REV
    GRADE_CODE                   GRADE
    ROUND_WEIGHT_LBS             CATCH.LBS
    LANDED_WEIGHT_LBS            LWT_LBS
    ADJUSTED_GEAR_CODE           ADJ_GRID
    DAHL_GROUNDFISH_CODE         DAHL_SECTOR
    IS_REMOVAL_LEGAL             LEGAL_REMOVAL
    IS_OVERAGE                   OVERAGE
    PRODUCT_FORM_CODE            PRODUCT_FROM
    PRODUCT_USE_CODE             PRODUCT_USE
    ORIG_PACFIN_CATCH_AREA_CODE  FTL_ARID
    ORIG_PACFIN_SPECIES_CODE     FTL_SPID
    DEALER_NUM                   PROC
    LANDING_MONTH                PERIOD
    ", quiet = TRUE, what = "", strip.white = TRUE),
    ncol = 2, byrow = TRUE)
  colnames(master) <- c("raw", "vdrfd")
  matches <- match(colnames(data), master[, "raw"])
  colnames(data) <- ifelse(is.na(matches),
    colnames(data),
    master[matches, use])
  return(data)
}