
##############################################################################
#
#  combineCALCOM combines CALCOM and PacFIN data (only used for Petrale).
#
##############################################################################

combineCALCOM = function ( Pdata, CALCOM ) {

  cat( "\nCombining CALCOM and PacFIN data \n\n" )

  # Fix dates

  cat( paste("PacFIN records:", nrow(Pdata)), "\n\n" )
  cat( paste("CALCOM records:", nrow(CALCOM)), "\n\n" )

  CALCOM$SAMPLE_DATE = as.character(CALCOM$SAMPLE_DATE)

  # Break out Year, Month, Day from vector formatted "2/23/2012"

  trueDate = as.Date(CALCOM$SAMPLE_DATE, format="%m/%d/%Y")

  CALCOM$SAMPLE_YEAR = as.numeric(format(trueDate, format="%Y"))
  CALCOM$SAMPLE_MONTH = as.numeric(format(trueDate, format="%m"))
  CALCOM$SAMPLE_DAY = as.numeric(format(trueDate, format="%d"))

  # Fix Areas

  CALCOM$PSMFC_AREA = NA
  CALCOM$PSMFC_AREA[CALCOM$PORT %in% c("ERK","CRS")] = "1C"
  CALCOM$PSMFC_AREA[CALCOM$PORT %in% c("BRG","OSF","MNT","1")] = "1B"
  CALCOM$PSMFC_AREA[CALCOM$PORT %in% c("OSB","MRO","2")] = "1A"

  # Create PacFIN format matrix and fill

  CAL.dat = as.data.frame(matrix(data=NA, nrow = nrow(CALCOM) , ncol = ncol(Pdata)))

  names(CAL.dat) = names(Pdata)

  CAL.dat$SPID         = CALCOM$SPECIES
  CAL.dat$SAMPLE_NO    = CALCOM$SAMPLE_NO
  CAL.dat$FISH_NO      = as.numeric(CALCOM$FISH_NO)
  CAL.dat$FISH_LENGTH  = as.numeric(CALCOM$TLENGTH)
  CAL.dat$SEX          = CALCOM$SEX
  CAL.dat$DEPTH_AVG    = as.numeric(CALCOM$DEPTH)
  CAL.dat$TOTAL_WGT    = as.numeric(CALCOM$TOTAL_WGT)
  CAL.dat$PORT         = CALCOM$PORT_COMPLEX
  CAL.dat$SAMPLE_YEAR  = as.numeric(CALCOM$SAMPLE_YEAR)
  CAL.dat$SAMPLE_MONTH = as.numeric(CALCOM$SAMPLE_MONTH)
  CAL.dat$SAMPLE_DAY   = as.numeric(CALCOM$SAMPLE_DAY)
  CAL.dat$SOURCE_AGID  = "CALCOM"
  CAL.dat$PSMFC_ARID   = "CALCOM"
  CAL.dat$SAMPLE_AGENCY = "CALCOM"
  CAL.dat$age1         = as.numeric(CALCOM$AGE)
  CAL.dat$age2         = as.numeric(CALCOM$AGE)
  CAL.dat$age3         = as.numeric(CALCOM$AGE)
  CAL.dat$FISH_AGE_YEARS_FINAL = as.numeric(CALCOM$AGE)
  CAL.dat$FREQ         = 1

  # No sample type or method, no INPFC_AREA, so give them values that are
  # retained by cleanPacFIN.

  CAL.dat$AGE_METHOD = "S"
  CAL.dat$SAMPLE_METHOD = "R"
  CAL.dat$SAMPLE_TYPE = "M"
  CAL.dat$INPFC_AREA = "CALCOM"

  CAL.dat$GRID = "CALCOM"

  # Fix SEX.  2=females, 1=males based on length distributions

  CAL.dat$SEX[CAL.dat$SEX=="1"] = "M"
  CAL.dat$SEX[CAL.dat$SEX=="2"] = "F"

  # Done.

  cat( paste("Combined dataset:", nrow(rbind(Pdata, CAL.dat)), "\n\n" ) )

  return(rbind(Pdata,CAL.dat))

} # End CombineCALCOM
