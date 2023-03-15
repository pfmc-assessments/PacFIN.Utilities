###################################################################
#
#
#' Combine CalCOM and PacFIN data
#'
#' \code{combineCalCOM} adds required PacFIN columns to CalCOM data, initializing them
#' to meaningful values as appropriate, translating from the CalCOM values when
#' they exist in a different format.
#' 
#' \subsection{Workflow}{
#' Intended to be run before \code{\link{cleanPacFIN}}.
#' }
#' 
#' @param Pdata a PacFIN dataset
#' @param CalCOM a CalCOM dataset
#' 
#' @return Returns a combined dataset in PacFIN format.
#' 
#' @export
#'
#' @details
#' 
#' Date formats and genders are translated to PacFIN format, PSMFC_AREAs are derived
#' from CalCOM "PORT"s.
#' 
#' Other data that aren't reported in CalCOM datasets (e.g., SAMPLE_TYPE and 
#' SAMPLE_METHOD) are initialized so that the CalCOM data aren't rejected by 
#' \code{cleanPacFIN}.
#'
#' @author Melissa Haltuch, Andi Stephens 
#
###################################################################

combineCalCOM = function ( Pdata, CalCOM ) {

  cat( "\nCombining CalCOM and PacFIN data \n\n" )

  # Fix dates

  cat( paste("PacFIN records:", nrow(Pdata)), "\n\n" )
  cat( paste("CalCOM records:", nrow(CalCOM)), "\n\n" )

  CalCOM$SAMPLE_DATE = as.character(CalCOM$SAMPLE_DATE)

  # Break out Year, Month, Day from vector formatted "2/23/2012"

  trueDate = as.Date(CalCOM$SAMPLE_DATE, format="%m/%d/%Y")

  CalCOM$SAMPLE_YEAR = as.numeric(format(trueDate, format="%Y"))
  CalCOM$SAMPLE_MONTH = as.numeric(format(trueDate, format="%m"))
  CalCOM$SAMPLE_DAY = as.numeric(format(trueDate, format="%d"))

  # Fix Areas

  CalCOM$PSMFC_AREA = NA
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("ERK","CRS")] = "1C"
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("BRG","OSF","MNT","1")] = "1B"
  CalCOM$PSMFC_AREA[CalCOM$PORT %in% c("OSB","MRO","2")] = "1A"

  # Create PacFIN format matrix and fill

  Cal.dat = as.data.frame(matrix(data=NA, nrow = nrow(CalCOM) , ncol = ncol(Pdata)))

  names(Cal.dat) = names(Pdata)

  Cal.dat$SPID         = CalCOM$SPECIES
  Cal.dat$SAMPLE_NO    = CalCOM$SAMPLE_NO
  Cal.dat$FISH_NO      = as.numeric(CalCOM$FISH_NO)
  Cal.dat$FISH_LENGTH  = as.numeric(CalCOM$TLENGTH)
  Cal.dat$SEX          = CalCOM$SEX
  Cal.dat$DEPTH_AVG    = as.numeric(CalCOM$DEPTH)
  Cal.dat$TOTAL_WGT    = as.numeric(CalCOM$TOTAL_WGT)
  Cal.dat$SPECIES_WGT  = as.numeric(CalCOM$SumOfWEIGHT)
  Cal.dat$PORT         = CalCOM$PORT_COMPLEX
  Cal.dat$SAMPLE_YEAR  = as.numeric(CalCOM$SAMPLE_YEAR)
  Cal.dat$SAMPLE_MONTH = as.numeric(CalCOM$SAMPLE_MONTH)
  Cal.dat$SAMPLE_DAY   = as.numeric(CalCOM$SAMPLE_DAY)
  Cal.dat$SOURCE_AGID  = "CalCOM"
  Cal.dat$PSMFC_ARID   = "CalCOM"
  Cal.dat$age1         = as.numeric(CalCOM$AGE)
  Cal.dat$age2         = as.numeric(CalCOM$AGE)
  Cal.dat$age3         = as.numeric(CalCOM$AGE)
  Cal.dat$FISH_AGE_YEARS_FINAL = as.numeric(CalCOM$AGE)
  Cal.dat$FREQ         = 1
  Cal.dat$FISH_LENGTH_TYPE = ""

  # No sample type or method, no INPFC_AREA, so give them values that are
  # retained by cleanPacFIN.

  Cal.dat$AGE_METHOD = "S"
  Cal.dat$SAMPLE_METHOD = "R"
  Cal.dat$SAMPLE_TYPE = "M"
  Cal.dat$INPFC_AREA = "CalCOM"

  Cal.dat$GRID = "CalCOM"

  # Fix SEX.  2=females, 1=males based on length distributions

  Cal.dat$SEX[Cal.dat$SEX=="1"] = "M"
  Cal.dat$SEX[Cal.dat$SEX=="2"] = "F"

  # Done.

  cat( paste("Combined dataset:", nrow(rbind(Pdata, Cal.dat)), "\n\n" ) )

  return(rbind(Pdata,Cal.dat))

} # End CombineCalCOM
