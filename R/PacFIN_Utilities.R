#############################################################################
#
#  Utilities for working up PacFIN Data.
#
#  Andi Stephens, 2012
#
##############################################################################

##############################################################################
#
# Function paste.col
#
# Converts a row to a string, "pasting" the columns together.
#
#############################################################################

paste.col <- function(x) {

  # If it's just a vector, return each value as a string.

  if (is.null(dim(x))) {

    return(paste(as.character(x)))

  } # End if

  # Otherwise, it's a matrix.

  # Get the value of the first column in character form

  out <- paste(as.character(x[, 1]))

  # Add on each of the rest of the columns

  for (i in 2:ncol(x)) {

   out <- paste(out, as.character(x[, i]))

  } # End for

  return(out)

} # End paste.col


#############################################################################
#
# Function find.matching.rows
#
#
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#   REVISED: Andi Stephens, 2010.
#
#   Takes two tables with a shared primary key, and
#   returns the rows of the second table for which the
#   keys match those of the first.
#
#   NOTE:  The way this is used assumes that the second table is a
#          superset of the first (i.e., that each value is matched).
#
#   Changes:
#
#        Changed name from original "match.f" to "find.matching.rows".
#
#        Removed sub-function 'paste.col' and made it standalone.
#
#        The matching function no longer modifies it's inputs, just
#        returns the values to be 'cbound' in the calling function.
#
#
# Using the primary keys in columns named 'findex' and 'tindex', finds the
# matching values for 'file' in 'table' and returns 'table' column(s) 'tcol'.
#
# Note that no test is made to determine if there are unmatched rows.
#
#############################################################################

find.matching.rows <- function(file, table, findex = 1, tindex = 1, tcol = 2, round. = T) {

  # Coerce a vector argument into a matrix

  if (is.null(dim(file))) {  dim(file) <- c(length(file), 1) }

  # If the primary keys are numeric, round them.

  if (round.) {

    if (is.numeric(file[, findex])) { file[, findex] <- round(file[, findex]) }

    if (is.numeric(table[, tindex])) { table[, tindex] <- round(table[, tindex]) }

  } # End if round.

  # Convert the indices to character strings for comparison, and get the
  # positions of the 'file' values in the 'table' values.

  matched.rows = match(paste.col(file[, findex]), paste.col(table[, tindex]))

  # Return the 'tcol' values in the rows of the 'table' that matched.

  return(table[matched.rows, tcol, drop = FALSE])

} # End function find.matching.rows

##############################################################################
#
#  cleanPacFIN filters out unusable data, and fixes lengths.
#
#  Fields to use post-cleaning are:
#
#  length
#
#  The original fields (in the retained data) are left untouched for
#  diagnostics.
#
##############################################################################

cleanPacFIN = function( Pdata,
                        only_USINPFC = FALSE,
			keep_INPFC = NULL,
			remove_INPFC = NULL,
                        badRecords = NULL,
                        keep_sample_type = c("", "M"),
                        keep_sample_method = "R",
			keep_missing_lengths = FALSE,
                        CLEAN=TRUE) {

  cat( "\nCleaning data\n\n" )

  if (!CLEAN) {

    cat("\nGenerating data report only.  No data will be removed.\n")

    Original_data = Pdata
  }

  # Define legal areas

  # This is a legacy comment from Owen's POP code.  Need to investigate.
  # NOTE only a few with "" from 2005 and 2010. NEARLY 100,000 are from CHR
  # and VCN (through 1978) - a large proportion of early data.

  USinpfc = c("VUS","CL","VN","COL","NC","SC","EU","CALCOM","CP","EK","MT","PS ")

  # Fix Lengths.  Use FISH_LENGTH if there is no FORK_LENGTH.

  Pdata$FORK_LENGTH[is.na(Pdata$FORK_LENGTH)] = -1
  Pdata$length = ifelse(Pdata$FISH_LENGTH > -1, Pdata$FISH_LENGTH, Pdata$FORK_LENGTH)

  # Convert mm to cm

  Pdata$lengthcm = floor(Pdata$length / 10)

  # Fix EXP_WT:  Used in expansions.  Zero works arithmetically, NA does not.

  Pdata$exp_wt = Pdata$EXP_WT
  Pdata$exp_wt[is.na(Pdata$EXP_WT)] = 0

  # We don't want no stinkin' NAs!

  Pdata$SEX[is.na(Pdata$SEX)] = "U"
  Pdata$SEX[Pdata$SEX == 0 ] = "U"

  # Flag records without a SAMPLE_NO

  Pdata$sample = Pdata$SAMPLE_NO

  # KFJ: use more values than just NA, also only do if TRUE
  # Andi:  thanks!

  flags <- c("NA", "Nan", "")

  if (any(Pdata$sample %in% flags)) {

    Pdata$sample[Pdata$sample %in% flags] <- "-1"

  } # End if

  # Remove records
  Rec_summary = rep(0,7)

  Rec_summary[1] = nrow(Pdata)

  if (only_USINPFC == TRUE) { Pdata = Pdata[Pdata$INPFC_AREA %in% USinpfc,] }

  if (! is.null(keep_INPFC) ) { Pdata = Pdata[Pdata$INPFC_AREA %in% keep_INPFC,] }
  if (! is.null(remove_INPFC) ) { Pdata = Pdata[!Pdata$INPFC_AREA %in% remove_INPFC,] }

  Rec_summary[2] = nrow(Pdata)

  Pdata = Pdata[!Pdata$sample %in% badRecords,]

  Rec_summary[3] = nrow(Pdata)

  if (! is.null(keep_sample_type)) { Pdata = Pdata[Pdata$SAMPLE_TYPE %in% keep_sample_type,] }

  Rec_summary[4] = nrow(Pdata)

  if (! is.null(keep_sample_method) ) { Pdata = Pdata[Pdata$SAMPLE_METHOD %in% keep_sample_method,] }

  Rec_summary[5] = nrow(Pdata)

  Pdata = Pdata[Pdata$SAMPLE_NO != -1,]

  Rec_summary[6] = nrow(Pdata)

  if (!keep_missing_lengths) { Pdata = Pdata[!is.na(Pdata$length),] }

  Rec_summary[7] = nrow(Pdata)

  # Report removals

  cat("\nRemoval Report\n\n")
  cat("Records in input:                 ", Rec_summary[1], "\n")
  cat("Records not in INPFC_AREA:        ", Rec_summary[1] - Rec_summary[2], "\n")
  cat("Records in badRecords list:       ", Rec_summary[2] - Rec_summary[3], "\n")
  cat("Records with bad SAMPLE_TYPE      ", Rec_summary[3] - Rec_summary[4], "\n")
  cat("Records with bad SAMPLE_METHOD    ", Rec_summary[4] - Rec_summary[5], "\n")
  cat("Records with no SAMPLE_NO         ", Rec_summary[5] - Rec_summary[6], "\n")
  cat("Records with no usable length     ", Rec_summary[6] - Rec_summary[7], "\n")
  cat("Records remaining:                ", nrow(Pdata), "\n\n")


  # Define fishyr, fleet, fishery and season  -- some assessments manipulate these.

  cat("These values have been initialized for use when comps are generated.\n")
  cat("Use Stratify and getSeason to reset them to appropriate values.\n\n")

  # KFJ: only create columns if they do not exist or if they are not numeric

  for ( i in c("fleet","fishery","season") ) {

    if (!i %in% colnames(Pdata)) {

      tmpcol = ncol(Pdata) + 1
      tmp = rep(1, nrow(Pdata))

      Pdata = cbind(Pdata, tmp)
      names(Pdata)[tmpcol] = i

      cat("Pdata$",i," = 1\n")

    } # End if

  } # End for

  Pdata$fishyr = Pdata$SAMPLE_YEAR
  cat("Pdata$fishyr is initialized to Pdata$SAMPLE_YEAR\n")

  if (CLEAN) {

    return(Pdata)

  } else {

    cat("\n\nReturning original data because CLEAN=FALSE\n\n")

    return(Original_data)

 } # End if-else

} # End cleanPacFIN

#############################################################################
#
# cleanAges removes the samples with bad ages or agemethods.  Depends on the
# data having first been filtered with cleanPacFIN().
#
#############################################################################

cleanAges = function( Pdata, keep_age_methods=c("B","S",""), minAge=0, maxAge=NULL, CLEAN=TRUE ) {

  cat( "\nCleaning Age data.\n\n")

  if ( length(Pdata$fishyr) == 0 ) {

    cat("\nWarning!  cleanPacFIN was not run on this data\n\n")

  } # End if

  # Fix Ages

  Pdata$age = Pdata$FISH_AGE_YEARS_FINAL
  Pdata$age[is.na(Pdata$age)] <- -1
  Pdata$age = ifelse(Pdata$age > 0, Pdata$age, Pdata$age1)
  Pdata$age[is.na(Pdata$age)] = -1
  Pdata$age = ifelse(Pdata$age > 0, Pdata$age, Pdata$age2)
  Pdata$age[is.na(Pdata$age)] = -1
  Pdata$age = ifelse(Pdata$age > 0, Pdata$age, Pdata$age3)
  Pdata$age[is.na(Pdata$age)] = -1

  # Fix Age Methods

  Pdata$agemethod = Pdata$AGE_METHOD
  Pdata$agemethod[Pdata$agemethod == "1"] = "B"
  Pdata$agemethod[Pdata$agemethod == "2"] = "S"
  Pdata$agemethod[is.na(Pdata$agemethod)] = -1

  if (!CLEAN) {

    cat("\nGenerating data report only.  No data will be removed.\n")

    Original_data = Pdata

  } # End if

  # Remove bad records

  Rec_summary = rep(0,3)

  Rec_summary[1] = nrow(Pdata)

  # Records with bad ages

  Pdata = Pdata[Pdata$age >= minAge,]
  Rec_summary[2] = nrow(Pdata)

  Pdata = Pdata[Pdata$agemethod %in% keep_age_methods,]
  Rec_summary[3] = nrow(Pdata)

  if ( ! is.null(maxAge) ) {

    cat("\nSetting maximum age to", maxAge, "\n")

    Pdata$age[Pdata$age > maxAge] = maxAge

  } # End if

  # Report removals

  cat("\nRemoval report\n\n")
  cat("Records in input:                  ", Rec_summary[1], "\n")
  cat("Records with age less than min:    ", Rec_summary[1] - Rec_summary[2], "\n")
  cat("Records with bad agemethods:       ", Rec_summary[2] - Rec_summary[3], "\n")
  cat("Records remaining:                 ", nrow(Pdata), "\n")

  if (CLEAN) {

    return(Pdata)

  } else {

    cat("\nReturning original data because CLEAN=FALSE\n\n")
    return(Original_data)

  } # End if-else

} # End cleanAges



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


#############################################################################
#
# getExpansion_2 is the second-stage expansion
#
# Calculate the stratified sampled biomass, All_Trips_Sampled_Lbs by summing
# Trip_Sampled_Lbs. Calculate the stratified catch by summing MT * 2204.
#
# The per-trip, per-stratum Expansion_Factor_2 is the catch / sampled catch.
#
#############################################################################

getExpansion_2 = function( Pdata, Catch, Convert=FALSE, maxExp=0.95 ) {

  # Start clean

  Pdata$Expansion_Factor_2 = NA

  if (length(Pdata$Trip_Sampled_Lbs) == 0) {

    stop("Please run getExpansion_1 first\n\n")

  } # End if


  # Pdata must have a "usegear" column encoding gear.  Paste together with state:  CA.TWL, OR.NONTWL
  # State and gear combination must match column names of Catch.

  if (length(Pdata$usegear) == 0) {

    stop("Input data must have gear type encoded in column 'usegear'\n\n")

  } # End if

  Pdata$state.gear = paste(Pdata$state, Pdata$usegear, sep=".")

  ############################################################################
  # Check catch against Pdata.
  ############################################################################

  # Catch is expected to have columns named for state and gear, as well as a "Year" column.

  Catchgears = sort(names(Catch)[2:length(names(Catch))])

  Pgears = sort(unique(Pdata$state.gear))

  if ( !identical(Pgears,Catchgears) ) {

    cat("Error:  mismatch between dataset and catch.\n\n")

    cat("Catch: ", Catchgears, "\n\n")
    cat("Data:  ", Pgears, "\n\n")

    stop()

  } # End if

  # Get summed sampled lbs per individual sample (tow).

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  # Get the totals by year and state.gear

  strat = c("fishyr", "state.gear")

  SumSampled = aggregate(tows$Trip_Sampled_Lbs, tows[, strat], sum, na.rm=T)

  names(SumSampled)[3] = "Sum_Sampled_Lbs"

  tows$Sum_Sampled_Lbs = find.matching.rows(tows, SumSampled, strat, strat,  "Sum_Sampled_Lbs")[[1]]

  # Convert Catch to lbs.

  if (Convert) {

    cat("Converting Catch to pounds (multiplying by 2204). \n\n")
    Catch[,2:length(Catchgears)] = Catch[,2:length(Catchgears)] * 2204

  } # End if

  # Matching is on Year == fishyr, Colname == state.gear.
  # Pdata$catch col gets the matched Catch.

  # Loop over state.gear then year, attaching the appropriate Catch

  tows$catch=NULL

  for ( sg in 2:ncol(Catch) ) {

    state.gear = names(Catch)[sg]

    for ( yr in Catch$Year ) {

      tows$catch[tows$fishyr == yr & tows$state.gear == state.gear ] = Catch[Catch$Year == yr, sg]

      cat(".")

    } # End for Year

    cat("Assigned catch for", state.gear, "\n")

  } # End for Catch

  NoCatchYr = tows$fishyr[is.na(tows$catch)]
  NoCatchSG = tows$state.gear[is.na(tows$catch)]
  NoCatch = cbind(NoCatchYr, NoCatchSG)
  NoCatch = NoCatch[! duplicated(NoCatch),]

  # KFJ - only print if an issue
  # Andi -- thanks!

  if (nrow(NoCatch) > 0) {

    cat("No Catch was found for these combinations:\n\n")
    print(NoCatch)
    cat("\n\n")

  } # End if

  # Now expansion is calculated by dividing the catch by the Sum_Sampled_Lbs.

  tows$EF2 = tows$catch/tows$Sum_Sampled_Lbs

  tows$EF2[tows$EF2 < 1] = 1
  tows$EF2[!is.finite(tows$EF2)] <- 1
  # Match EF2 to the larger dataset.

  # Scale up from tows to Pdata

  Pdata$Sum_Sampled_Lbs = find.matching.rows(Pdata, tows, strat, strat,  "Sum_Sampled_Lbs")[[1]]
  Pdata$catch = find.matching.rows(Pdata, tows, strat, strat,  "catch")[[1]]
  Pdata$Expansion_Factor_2 = find.matching.rows(Pdata, tows, strat, strat,  "EF2")[[1]]

  cat("Summary of Expansion_Factor_2\n\n")
  print(summary(Pdata$Expansion_Factor_2))

  NA_EF2 = Pdata[is.na(Pdata$Expansion_Factor_2),]

  nNA = nrow(NA_EF2)

  if (nNA > 0) {

    barplot(xtabs(NA_EF2$FREQ ~ NA_EF2$state + NA_EF2$fishyr),
            col=rainbow(3),
            legend.text = T, xlab = "Year", ylab = "Samples",
            main="NA Expansion_Factor_2 replaced by 1")

  } # End if

  cat("\n", nNA, "NA Expansion_Factor_2 values replaced by 1.\n\n")

  Pdata$Expansion_Factor_2[is.na(Pdata$Expansion_Factor_2)] = 1

  Pdata$Expansion_Factor_2 = capValues(Pdata$Expansion_Factor_2, maxExp)

  cat("Summary of Expansion_Factor_2\n\n")
  print(summary(Pdata$Expansion_Factor_2))

  boxplot(Pdata$Expansion_Factor_2 ~ Pdata$fishyr, main="Expansion_Factor_2")

  cat("\nRemember to set (or reset) Pdata$Final_Sample_Size\n\n")

  return(Pdata)

} # End function getExpansion_2




##############################################################################
#
# doDiags creates diagnostic plots and summaries, writing them to a file
# in addition to plotting onscreen and console.
#
##############################################################################

doDiags = function( Pdata, fname=NULL ) {

  cat( "\nRunning diagnostics\n\n" )

  if ( is.null(fname) ) {

    # Set up filenames for txt, pdf

    species = sort(unique(Pdata$SPID))
    pdffile = paste( "Diags.", species, ".pdf", sep="")

  } else {

    pdffile = paste(fname, ".pdf", sep="")

  } # End ifelse

  cat( "Plots will be written to", pdffile, "\n" )

  par(mfrow=c(2,2))

  # Develop statistics of interest

  len = Pdata[!is.na(Pdata$FISH_LENGTH),]
  len$len = floor(len$FISH_LENGTH/10)
  len$depth_mid = (len$DEPTH_MIN+len$DEPTH_MAX)/2
  ltows = len[!duplicated(len$SAMPLE_NO),]

  meanLen.yr = tapply(len$len,list(len$SAMPLE_YEAR),mean)
  meanLen = tapply(len$len,list(len$SAMPLE_NO,len$SAMPLE_YEAR),mean)

  age = Pdata[!is.na(Pdata$FISH_AGE_YEARS_FINAL),]
  age$age = age$FISH_AGE_YEARS_FINAL
  atows = age[!duplicated(age$SAMPLE_NO),]
  meanAge = tapply(age$age,list(age$SAMPLE_NO,age$SAMPLE_YEAR),mean)

  # Print tables

  #cat("Lengths for which FISH_LENGTH_TYPE is T:  ")
  #print(len[len$FISH_LENGTH_TYPE=="T",])
  #cat("\n\n")

  cat("Records per SAMPLE_YEAR\n\n")
  print(table(Pdata$SAMPLE_YEAR,useNA="ifany"))
  cat("\n\n")

  cat("SOURCE_AGID vs. SAMPLE_AGENCY\n")
  print(table(Pdata$SOURCE_AGID,Pdata$SAMPLE_AGENCY,useNA="ifany"))
  cat("\n\n")

  cat("FISH_LENGTH_TYPE\n")
  print(table(Pdata$FISH_LENGTH_TYPE,useNA="ifany"))
  cat("\n\n")

  cat("FISH_LENGTH\n")
  print(table(Pdata$FISH_LENGTH,useNA="ifany"))
  cat("\n\n")

  cat("GEAR vs GRID\n")
  print(table(len$GEAR,len$GRID))
  cat("\n\n")

  cat("FISH_LENGTH for lengthed fish\n")
  print(table(len$FISH_LENGTH_TYPE,useNA="ifany"))
  cat("\n\n")

  cat("SAMPLE_YEAR vs SOURCE_AGID for lengthed fish\n")
  print(table(len$SAMPLE_YEAR,len$SOURCE_AGID))
  cat("\n\n")

  cat("Difference between FISH_LENGTH and floor(FISH_LENGTH)\n")
  print(table(len$FISH_LENGTH-floor(len$FISH_LENGTH)))
  cat("\n\n")

  cat("Difference between FISH_LENGTH/10 and floor(FISH_LENGTH/10)\n")
  print(table(round(len$FISH_LENGTH/10-floor(len$FISH_LENGTH/10),1)))
  cat("\n\n")

  cat("DEPTH_AVG for lengthed fish\n")
  print(table(is.na(len$DEPTH_AVG)))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for SAMPLE_NOs with lengthed fish\n")
  print(table(ltows$SAMPLE_YEAR,ltows$SOURCE_AGID))
  cat("\n\n")

  cat("DEPTH_AVG for SAMPLE_NOs with lengthed fish\n")
  print(table(is.na(ltows$DEPTH_AVG),useNA="ifany"))
  cat("\n\n")

  cat("Number of aged fish\n")
  print(nrow(age))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for SAMPLE_NOs with aged fish\n")
  print(table(atows$SAMPLE_YEAR,atows$SOURCE_AGID))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for aged fish\n")
  print(table(age$SAMPLE_YEAR,age$SOURCE_AGID))
  cat("\n\n")

  cat("age2 vs. age3 for aged fish\n")
  print(table(age$age2,age$age3,useNA="ifany"))
  cat("\n\n")

  cat("age1 vs. age2 for aged fish\n")
  print(table(age$age1,age$age2,useNA="ifany"))
  cat("\n\n")

  # Plots

  # Fingers crossed, works the same for Mac and PC

  # Set up device for pdf

  graphics.off()

  pdf(pdffile)

  par(mfrow=c(2,2))

  hist(len$len,nclass=30, xlab="", main="FISH_LENGTH")

  barplot(table(10*round(len$FISH_LENGTH/10-floor(len$FISH_LENGTH/10),1)),
          xlab="Difference in rounded and floored lengths")

  plot(len$FISH_LENGTH,len$FORK_LENGTH,pch=16, xlab="FISH_LENGTH", ylab="FORK_LENGTH")

  plot(len$DEPTH_AVG,len$depth_mid,xlim=c(0,400),ylim=c(0,400), xlab="DEPTH_AVG", ylab="Depth_mid")
  abline(a=0,b=1)

  hist(ltows$DEPTH_AVG, xlab="", main="DEPTH_AVG")

  hist(age$age,nclass=30, xlab="", main="Age")

  par(mfrow=c(2,1))
  boxplot(as.list(as.data.frame(meanLen)),varwidth=T,main="Mean length")
  boxplot(as.list(as.data.frame(meanAge)),varwidth=T,main="Mean age")

  dev.off()

} # End doDiags

#
# That's All, Folks!
#
##############################################################################
