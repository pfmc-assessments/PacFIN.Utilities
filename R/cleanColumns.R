#' Change Column Names to Vdrfd
#'
#' Clean columns of a data frame from PacFIN.
#' All description columns are removed.
#' Should you want anything different, please feel free to
#' post an issue on github, email the package maintainer, or submit a
#' pull request.
#'
#' @template data
#'
#' @export
#' @author Kelli Faye Johnson
#' @return A data frame with fewer columns and some names changed.
#'
cleanColumns <- function(data) {
  if ("LANDING_YEAR" %in% colnames(data)) {
    data <- cleanColumns.catch(data)
  }
  if ("SAMPLE_YEAR" %in% colnames(data)) {
    data <- cleanColumns.bds(data)
  }

  data <- data %>%
    dplyr::select(-dplyr::matches("DESC|COLUMN")) %>%
    data.frame()

  return(data)
}
#'
#' Clean columns of PacFIN biological data
#'
#' Many column names are changed in the biological data to match what was
#' used in legacy code. As time goes on the code will be reworked to use the
#' new name coming from PacFIN. Columns not used in the code are removed.
#'
#' @template data
#' @return A data frame with fewer columns and some column names changed.
#'
cleanColumns.bds <- function(data) {

  master <- matrix(scan(text = "
    Comp_FT                      vdrfd
    ADJUSTED_CLUSTER_WEIGHT_LBS  ADJ_CLWT
    AGENCY_AGE_STRUCTURE_CODE    AGE_STRUCT_AGCODE
    AGENCY_CODE                  SOURCE_AGID
    AGENCY_CONDITION_CODE        COND_AGCODE
    AGENCY_GEAR_CODE             GEAR
    AGENCY_GRADE_CODE            GRADE_AGCODE
    AGENCY_FISH_MATURITY_CODE    MATURITY_AGCODE
    AGENCY_PORT_CODE             PORT
    AGE_METHOD_CODE              AGE_METHOD
    CLUSTER_SEQUENCE_NUMBER      CLUSTER_NO
    CLUSTER_WEIGHT_LBS           CLUSTER_WGT
    DATE_AGE_RECORDED            DATE_AGED
    DEPTH_AVERAGE_FATHOMS        DEPTH_AVG
    DEPTH_MAXIMUM_FATHOMS        DEPTH_MAX
    DEPTH_MINIMUM_FATHOMS        DEPTH_MIN
    EXPANDED_SAMPLE_WEIGHT       EXP_WT
    FINAL_FISH_AGE_CODE          FISH_AGE_CODE_FINAL
    FINAL_FISH_AGE_IN_YEARS      FISH_AGE_YEARS_FINAL
    FISH_LENGTH_TYPE_CODE        FISH_LENGTH_TYPE
    FISH_MATURITY_CODE           MATURITY
    FISH_SEQUENCE_NUMBER         FISH_NO
    FORK_LENGTH_IS_ESTIMATED     FORK_LENGTH_ESTIMATED
    FRAME_CLUSTER_WEIGHT_LBS     FRAME_CLWT
    INPFC_AREA_TYPE_CODE         INPFC_ARID
    OBSERVED_FREQUENCY           FREQ
    PACFIN_CONDITION_CODE        COND
    PACFIN_PORT_CODE             PCID
    PACFIN_GRADE_CODE            GRADE
    PACFIN_GEAR_CODE             GRID
    PACFIN_SPECIES_CODE          SPID
    PERSON_WHO_AGED              AGED_BY
    PSMFC_CATCH_AREA_CODE        PSMFC_ARID
    SAMPLE_METHOD_CODE           SAMPLE_METHOD
    SAMPLE_NUMBER                SAMPLE_NO
    SEX_CODE                     SEX
    SPECIES_WEIGHT_LBS           SPECIES_WGT
    VESSEL_NUM                   DRVID
    WEIGHT_OF_FEMALES_LBS        FEMALES_WGT
    WEIGHT_OF_MALES_LBS          MALES_WGT
    WEIGHT_OF_LANDING_LBS        TOTAL_WGT
    ",
    quiet = TRUE, what = "", strip.white = TRUE),
    ncol = 2, byrow = TRUE)
  colnames(master) <- c("raw", "vdrfd")
  matches <- match(colnames(data), master[, "raw"])
  colnames(data) <- ifelse(is.na(matches),
    colnames(data),
    master[matches, "vdrfd"])

  # CRW: Columns have been collapsed to have 
  # TOTAL_WGT for CA and RWT_LBS for WA.
  data$RWT_LBS <- data$TOTAL_WGT
  data <- data %>%
    dplyr::select(-dplyr::matches("VESSEL|AGE_[SR]|^NUM|LOAD|COMMON|_ID|agedby"))

  return(data)
}

#' Clean columns of PacFIN catch data
#'
#' Clean columns of a data frame from PacFIN that contains catch data.
#' The original column names are retained as there was no legacy code
#' for catch data. Many columns are removed and only those based on
#' the call to [dplyr::select] are kept.
#'
#' @template data
#' @return A data frame with fewer columns.
#'
cleanColumns.catch <- function(data) {

  #### REMOVE columns that are redundant and make things cluttered
  data <- data %>%
    dplyr::select(dplyr::matches("LANDING|AGENCY|GEAR|AREA|_MT|_LBS|PORT|^[RCF].+_CODE"))

  return(data)

}
