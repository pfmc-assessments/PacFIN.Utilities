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

  cat( "\nCombining CalCOM and PacFIN data \n\n" )

  # Fix dates

  cat( paste("PacFIN records:", nrow(Pdata)), "\n\n" )
  cat( paste("CalCOM records:", nrow(CalCOM)), "\n\n" )

  CalCOM$SAMPLE_DATE <- as.character(CalCOM$SAMPLE_DATE)

  # Break out Year, Month, Day from vector formatted "2/23/2012"

  trueDate <- as.Date(CalCOM$SAMPLE_DATE, format="%m/%d/%Y")

  CalCOM$SAMPLE_YEAR <- as.numeric(format(trueDate, format = "%Y"))
  CalCOM$SAMPLE_MONTH <- as.numeric(format(trueDate, format = "%m"))
  CalCOM$SAMPLE_DAY <- as.numeric(format(trueDate, format = "%d"))

  # Fix Areas

  CalCOM$PSMFC_AREA <- NA
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("ERK","CRS")] <- "1C"
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("BRG","OSF","MNT","1")] <- "1B"
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("OSB","MRO","2")] <- "1A"

  # Create PacFIN format matrix and fill

  Cal.dat <- as.data.frame(matrix(data=NA, nrow = nrow(CalCOM) , ncol = ncol(Pdata)))

  names(Cal.dat) = names(Pdata)

  Cal.dat$SPID         <- CalCOM$SPECIES
  Cal.dat$SAMPLE_NO    <- CalCOM$SAMPLE_NO
  Cal.dat$FISH_NO      <- as.numeric(CalCOM$FISH_NO)
  Cal.dat$FISH_LENGTH  <- as.numeric(CalCOM$TLENGTH)
  Cal.dat$SEX          <- CalCOM$SEX
  Cal.dat$DEPTH_AVG    <- as.numeric(CalCOM$DEPTH)
  Cal.dat$TOTAL_WGT    <- as.numeric(CalCOM$TOTAL_WGT)
  Cal.dat$SPECIES_WGT  <- as.numeric(CalCOM$SumOfWEIGHT)
  Cal.dat$PORT         <- CalCOM$PORT_COMPLEX
  Cal.dat$SAMPLE_YEAR  <- as.numeric(CalCOM$SAMPLE_YEAR)
  Cal.dat$SAMPLE_MONTH <- as.numeric(CalCOM$SAMPLE_MONTH)
  Cal.dat$SAMPLE_DAY   <- as.numeric(CalCOM$SAMPLE_DAY)
  Cal.dat$SOURCE_AGID  <- "CalCOM"
  Cal.dat$PSMFC_ARID   <- "CalCOM"
  Cal.dat$age1         <- as.numeric(CalCOM$AGE)
  Cal.dat$FISH_AGE_YEARS_FINAL <- as.numeric(CalCOM$AGE)
  Cal.dat$FREQ         <- 1
  Cal.dat$FISH_LENGTH_TYPE <- ""
  # IGT: FISH_LENGTH_UNITS = "MM" were added outside this function for Petrale 
  # in 2023, but I don't know if all CalCOM data are in MM
  if ("FISH_LENGTH_UNITS" %in% names(CalCOM)) {
    Cal.dat$FISH_LENGTH_UNITS <- CalCOM$FISH_LENGTH_UNITS
  }

  # No sample type or method, no INPFC_AREA, so give them values that are
  # retained by cleanPacFIN.

  # IGT: assigning surface but 1985-1989 samples in 2019 Petrale 
  # were assigned a combination of surface and break-and-burn
  Cal.dat$AGE_METHOD1 <- "S" 
  Cal.dat$SAMPLE_METHOD <- "R"
  Cal.dat$SAMPLE_TYPE <- "M"
#  Cal.dat$INPFC_AREA = "CalCOM" # IGT: not present in PacFIN table 15 March 2023

  Cal.dat$GRID <- "CalCOM"

  # Fix SEX.  2=females, 1=males based on length distributions

  Cal.dat$SEX[Cal.dat$SEX == "1"] <- "M"
  Cal.dat$SEX[Cal.dat$SEX == "2"] <- "F"

  # Done.
  cat( paste("Combined dataset:", nrow(rbind(Pdata, Cal.dat)), "\n\n" ) )

  return(rbind(Pdata,Cal.dat))

} # End CombineCalCOM
