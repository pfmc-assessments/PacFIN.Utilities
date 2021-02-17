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
    ADJUSTED_CLUSTER_WEIGHT_LBS  ADJ_CLWT
    ADJUSTED_GEAR_CODE           ADJ_GRID
    AGENCY_AGE_STRUCTURE_CODE    AGE_STRUCT_AGCODE
    AGENCY_CODE                  SOURCE_AGID
    AGID                         SOURCE_AGID
    AGENCY_CONDITION_CODE        COND_AGCODE
    AGENCY_GEAR_CODE             GEAR
    AGENCY_GRADE_CODE            GRADE_AGCODE
    AGENCY_FISH_MATURITY_CODE    MATURITY_AGCODE
    AGENCY_PORT_CODE             PORT
    AGE_METHOD_CODE              AGE_METHOD
    CLUSTER_SEQUENCE_NUMBER      CLUSTER_NO
    CLUSTER_WEIGHT_LBS           CLUSTER_WGT
    CONDITION_CODE               COND
    COUNCIL_CODE                 COUNCIL
    DAHL_GROUNDFISH_CODE         DAHL_SECTOR
    DATE_AGE_RECORDED            DATE_AGED
    DEALER_NUM                   PROC
    DEPTH_AVERAGE_FATHOMS        DEPTH_AVG
    DEPTH_MAXIMUM_FATHOMS        DEPTH_MAX
    DEPTH_MINIMUM_FATHOMS        DEPTH_MIN
    DISPOSITION_CODE             DISP
    EXPANDED_SAMPLE_WEIGHT       EXP_WT
    EXVESSEL_REVENUE             REV
    FINAL_FISH_AGE_CODE          FISH_AGE_CODE_FINAL
    FINAL_FISH_AGE_IN_YEARS      FISH_AGE_YEARS_FINAL
    FISH_LENGTH_TYPE_CODE        FISH_LENGTH_TYPE
    FISH_MATURITY_CODE           MATURITY
    FISH_SEQUENCE_NUMBER         FISH_NO
    FLEET_CODE                   FLEET
    FORK_LENGTH_IS_ESTIMATED     FORK_LENGTH_ESTIMATED
    FRAME_CLUSTER_WEIGHT_LBS     FRAME_CLWT
    GRADE_CODE                   GRADE
    INPFC_AREA_TYPE_CODE         INPFC_ARID
    IS_IFQ_LANDING               IFQ_LANDING
    IS_OVERAGE                   OVERAGE
    IS_REMOVAL_LEGAL             LEGAL_REMOVAL
    LANDED_WEIGHT_LBS            LWT_LBS
    LANDING_DATE                 TDATE
    LANDING_MONTH                PERIOD
    LANDING_YEAR                 YEAR
    NUMBER_OF_FEMALES            FEMALES_NUM
    NUMBER_OF_MALES              MALES_NUM
    OBSERVED_FREQUENCY           FREQ
    ORIG_PACFIN_CATCH_AREA_CODE  FTL_ARID
    ORIG_PACFIN_SPECIES_CODE     FTL_SPID
    PACFIN_CATCH_AREA_CODE       ARID
    PACFIN_CONDITION_CODE        COND
    PACFIN_PORT_CODE             PCID
    PACFIN_GRADE_CODE            GRADE
    PACFIN_GEAR_CODE             GRID
    PACFIN_GROUP_GEAR_CODE       GRGROUP
    PACFIN_PORT_CODE             PCID
    PACFIN_SPECIES_CODE          SPID
    PACFIN_SPECIES_ID            SPID
    PARTICIPATION_GROUP_CODE     PARGRP
    PERSON_WHO_AGED              AGED_BY
    PORT_CODE                    PORT
    PRODUCT_FORM_CODE            PRODUCT_FROM
    PRODUCT_USE_CODE             PRODUCT_USE
    PSMFC_CATCH_AREA_CODE        PSMFC_ARID
    REMOVAL_TYPE_CODE            REMOVAL_TYPE
    ROUND_WEIGHT_LBS             CATCH.LBS
    SAMPLE_METHOD_CODE           SAMPLE_METHOD
    SAMPLE_NUMBER                SAMPLE_NO
    SEX_CODE                     SEX
    SPECIES_WEIGHT_LBS           SPECIES_WGT
    VESSEL_NUM                   DRVID
    WEIGHT_OF_FEMALES_LBS        FEMALES_WGT
    WEIGHT_OF_MALES_LBS          MALES_WGT
    WEIGHT_OF_LANDING_LBS        lwt_lbs
    ",
    quiet = TRUE, what = "", strip.white = TRUE),
    ncol = 2, byrow = TRUE)
  colnames(master) <- c("raw", "vdrfd")
  matches <- match(colnames(data), master[, "raw"])
  colnames(data) <- ifelse(is.na(matches),
    colnames(data),
    master[matches, use])

    # CRW: Above the WEIGHT_OF_LANDING_LBS was being assinged twice with the 
    # second assingment coming through (RWT_LBS). Also assigning this value
    # to TOTAL_WGT since these two columns have been collapsed to have 
    # TOTAL_WGT for CA and RWT_LBS for WA.
    data$RWT_LBS <- data$TOTAL_WGT <- data$lwt_lbs 

  return(data)
}
