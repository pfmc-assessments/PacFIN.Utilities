#' Combine CalCOM and PacFIN data
#'
#' `r lifecycle::badge("experimental")` Adds required PacFIN columns to CalCOM
#' data, initializing them to meaningful values as appropriate and translating
#' from the CalCOM values when they exist in a different format.
#'
#' @param Pdata Output from [PullBDS.PacFIN()].
#' @param CalCOM A data frame supplied, more than likely be E.J. Dick or
#'   California Department of Fish and Wildlife containing biological samples
#'   for flatfish species prior to 1990.
#'
#' @return Returns a combined dataset in PacFIN format.
#'
#' @export
#'
#' @details
#' This function was made defunct in March 2021 in [commit
#' d5c0355](https://github.com/pfmc-assessments/PacFIN.Utilities/commit/d5c0355795d10b945a9e28bfb05cb6811697c852)
#' because CalCOM data were expected to be available in PacFIN and restored in
#' April 2023 after discovering that CalCOM data for flatfish prior to 1990 is
#' not currently available in PacFIN, as discussed in [issue #101](
#' https://github.com/pfmc-assessments/PacFIN.Utilities/issues/101) because the
#' early data are "in separate tables because they are bin samples (# of fish)
#' rather than cluster samples (weight based). PacFIN doesn't access those
#' tables."
#'
#' Coding of data from CalCOM to PacFIN is done for dates, genders, and areas.
#' `PSMFC_AREA`s are derived from CalCOM `"PORT"`s.
#'
#' Other data that are not reported in CalCOM datasets (e.g., `SAMPLE_TYPE` and
#' `SAMPLE_METHOD`) are initialized to typical default values kept when cleaning
#' data so that the CalCOM data aren't rejected by [cleanPacFIN()].
#'
#' @author Melissa Haltuch, Andi Stephens, and Ian G. Taylor
#' @seealso
#' * [cleanPacFIN()], which should be ran after this function

combineCalCOM <- function(Pdata, CalCOM) {
  calcom_columns_example <- c(
    "SPECIES", "SAMPLE_NO", "SAMPLE_DATE", "AGE", "FISH_NO",
    "TLENGTH", "SEX", "DEPTH", "SumOfWEIGHT", "SumOfTOTAL_CT",
    "TOTAL_WGT", "PORT_COMPLEX"
  )
  check_column_names <- purrr::map_lgl(
    colnames(CalCOM),
    .f = ~ .x %in% calcom_columns_example
  ) %>%
    all()
  if (!check_column_names) {
    stop(
      "There are columns in CalCOM that cannot currently be ",
      "processed by combineCalCOM(), the following are viable options:\n",
      paste(calcom_columns_example, collapse = "\n")
    )
  }
  # Break out Year, Month, Day from vector formatted "2/23/2012"
  trueDate <- as.Date(
    x = as.character(CalCOM$SAMPLE_DATE),
    format = "%m/%d/%Y"
  )
  CalCOM$SAMPLE_YEAR <- as.numeric(format(trueDate, format = "%Y"))
  CalCOM$SAMPLE_MONTH <- as.numeric(format(trueDate, format = "%m"))
  CalCOM$SAMPLE_DAY <- as.numeric(format(trueDate, format = "%d"))

  # Fix Areas
  name_area <- "PSMFC_CATCH_AREA_CODE"
  CalCOM[[name_area]] <- NA
  CalCOM[[name_area]][CalCOM$PORT %in% c("ERK", "CRS")] <- "1C"
  CalCOM[[name_area]][CalCOM$PORT %in% c("BRG", "OSF", "MNT", "1")] <- "1B"
  CalCOM[[name_area]][CalCOM$PORT %in% c("OSB", "MRO", "2")] <- "1A"

  # Create PacFIN format matrix and fill
  CalCOM <- CalCOM %>%
    dplyr::rename(
      PACFIN_SPECIES_CODE = "SPECIES",
      SAMPLE_NUMBER = "SAMPLE_NO",
      FISH_SEQUENCE_NUMBER = "FISH_NO",
      FISH_LENGTH = "TLENGTH",
      DEPTH_AVERAGE_FATHOMS = "DEPTH",
      AGENCY_PORT_CODE = "PORT_COMPLEX",
      # landed weight for this sample in pounds
      WEIGHT_OF_LANDING_LBS = "TOTAL_WGT",
      CLUSTER_WEIGHT_LBS = "SumOfWEIGHT",
    ) %>%
    dplyr::mutate(
      SEX_CODE = nwfscSurvey::codify_sex(SEX),
      AGENCY_CODE = "CalCOM",
      age1 = as.numeric(AGE),
      # TODO what age method are calCOM ages
      AGE_METHOD1 = "",
      FISH_LENGTH_TYPE_CODE = "T",
      FISH_LENGTH_UNITS = "MM",
      OBSERVED_FREQUENCY = 1,
      FINAL_FISH_AGE_IN_YEARS = age1,
      PACFIN_PORT_CODE = dplyr::case_when(
        # TODO: what are calCOM port complexes 1 and 2
        AGENCY_PORT_CODE == 1 ~ "",
        AGENCY_PORT_CODE == 2 ~ "",
        .default = AGENCY_PORT_CODE
      ),
      SPECIES_WEIGHT_LBS = CLUSTER_WEIGHT_LBS,
      SAMPLE_METHOD_CODE = "R",
      SAMPLE_TYPE = "M",
      PACFIN_GEAR_CODE = "GFT",
      AGENCY_GEAR_CODE = PACFIN_GEAR_CODE
    ) %>%
    dplyr::select(-SAMPLE_DATE, -AGE, -SEX, -SumOfTOTAL_CT)

  # Determine what colnames are in Pdata
  CalCOM <- if (check_columns_downloaded(Pdata)) {
    CalCOM
  } else {
    changecol_pacfin(CalCOM)
  }
  out <- dplyr::full_join(
    x = Pdata,
    y = CalCOM,
    by = intersect(colnames(Pdata), colnames(CalCOM))
  )

  cli::cli_alert("Users must set AGE_METHOD for CalCOM data manually.")
  return(out)
}

check_pacfin_species_code_calcom <- function(x) {
  species_codes_in_calcom_bin <- c("DOVR", "EGLS", "PTRL", "REX")
  purrr::map_lgl(species_codes_in_calcom_bin, .f = ~ .x %in% x) %>%
    any()
}
