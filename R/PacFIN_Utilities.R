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
#  getSeason adds a field for season to the data.  Several seasonal schemes
#  may be provided, including the Petrale seasons ( 1 = winter months, 2 else ).
#
#  Other schemes may be provided as needed.
#
#  The yearUp and yearDown can be used to provide a list of SAMPLE_MONTHS
#  (1 through 12) for which to adjust the fishyr (the fishing year) up or down.
#  In other words, the winter months might belong to the following year, or to
#  the previous year:
#
#      yearUP = c(11,12)    or    yearDown = c(1,2)
#
##############################################################################

getSeason = function ( Pdata, season_type=-1, yearUp=NULL, yearDown=NULL, plotResults=F) {

  cat( "\nDefault season = 1\n\n")

  Pdata$season = 1

  if ( season_type == 0 ) {

    cat( "Assigning season from SAMPLE_MONTH\n\n")

    Pdata$season = as.numeric(Pdata$SAMPLE_MONTH)

  } # End if

  # Petrale seasons

  if ( season_type == 1 ) {

    cat("Assigning season ala Petrale; winter is season 1, summer is 2.\n\n")

    Pdata$season = 2
    Pdata$season[Pdata$SAMPLE_MONTH %in%  c(11,12,1,2)] = 1

  } # End if Petrale

  if (! is.null(yearUp)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] + 1

    cat("Incremented fishyr for months", yearUp, "to the next year.\n\n")

  } # End if yearUp

  if (! is.null(yearDown)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] - 1

    cat("Decremented fishyr for months", yearDown, "to the previous year.\n\n")

  } # End if yearDown

  if ( plotResults ) {

    nSeas = length(unique(Pdata$season))
    tmp = xtabs(Pdata$FREQ ~ Pdata$season + Pdata$fishyr)
    barplot(tmp, col=rainbow(nSeas), legend.text=paste("Season", rownames(tmp)),
            main=unique(Pdata$SPID), xlab="Year", ylab="Samples")

  } # End if

  return( Pdata )

} # End getSeason

##############################################################################
#
# Stratify takes an input vector and list of values used to designate
# the values returned in the strats vector.
#
# Any values not given in splits will be assigned to stratum 0.
#
# If numeric=T, this defaults to a call to findInterval, and stratum 0
# will be assigned to values smaller than the first value in splits.
#
# Note that this can be used to designate fleet or for stratification
# based on depth or INPFC area.
#
##############################################################################

Stratify = function ( inVector=NULL, splits=NULL, names=NULL, numeric=F ) {

  stratified = rep(0, length(inVector))

  if ( !is.null(names) ) {

    if ( length(names) != length(splits) ) {

      cat("Names vector is not the same length as splits")

      return(NA)

    } # End if


  } # End if

  if (!numeric) {

    inVector = as.character(inVector)

    for ( i in 1:length(splits) ) {

      splitby = as.character(splits[[i]])

      if (is.null(names)) {

        stratified[inVector %in% splitby] = i

      } else {

       stratified[inVector %in% splitby] = names[i]

      } # End if

    } # End for

  } else {

    stratfied = findInterval(inVector, splits)

  } # End if-else

  return(stratified)

} # End function Stratify


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

##############################################################################
#
# capValues takes a data column and a capping factor, which may be either
# a quantile (less than 1), or a value (greater than 1), and reduces outsize
# values in the data to the value given, or to the value of the dataset at
# that quantile.
#
# For example, the default 0.95 quantile in a dataset with maximum value 10
# might be 8.5.  Then all values greater than 8.5 will be reduced to 8.5.
#
# If a value is given, for example 9.3, then all values greater than 9.3
# will be reset to 9.3.
#
# A copy of the input data column with the modified values is returned.
#
##############################################################################

capValues = function( DataCol, maxVal=0.95 ) {

  if ( maxVal > 1 ) {

    max.val = maxVal
    cat("\nMaximum value capped at", max.val, "\n\n")

  } else {

    max.val = quantile(DataCol, maxVal, na.rm=T)
    cat("\nMaximum expansion capped at", maxVal, "quantile:", max.val, "\n\n")

  } # End if-else

  DataCol[DataCol > max.val] = max.val

  return(DataCol)

} # End function capValues

##############################################################################
#
# EF1_Denominator calculates the denominator for the level-1 expansion factor,
# the weight of sampled fish in the tow.
#
# Indiv_Wgts controls whether or not individual Length-Weight calculations will
# be used.
#
# The weight of the fish is calculated three ways.
#
# First, FEMALES_WGT and MALES_WGT are summed per SAMPLE_NO.
#
# The SPECIES_WGT is summed across all clusters in a sample to provide
# the second per SAMPLE_NO weight.
#
# Finally, weights of the male and female fish are calculated from their
# lengths.  These are summed per SAMPLE_NO to provide a per-sample weight.
# Zero weights might occur for some fish. These are filled in with the
# median weight of fish in the sample.
#
# Arguments:
#
#    Pdata              the data set.
#    Indiv_Wgts         whether or not to calculate individual fish weights
#                       from a length-weight relationship.
#
#    fa, fb             the coefficient and factor for female Length-Weights.
#    ma, mb             the coefficient and factor for male Length-Weights.
#    ua, ub             the coefficient and factor for unsexed Length-Weights.
#
# New columns:
#
#    Wt_Sampled_1	per-SAMPLE_NO summed MALES_WGT + FEMALES_WGT
#    Wt_Sampled_2	per-SAMPLE_NO summed cluster_wt
#    LW_Calc_Wt		individual weights predicted from L-W
#    Wt_Sampled_3	per-SAMPLE_NO summed LW_Calc_Wts
#
#    Wt_Sampled		per-SAMPLE_NO combined weights.
#                       This is preferentially Wt_Sampled_1, with NAs
#                       replaced with values from Wt_Sampled_2, and NAs
#			remaining replaced with values from Wt_Sampled_3.
#
#    Wt_Method		Denotes by which method the Wt_Sampled was filled.
#
#
##############################################################################

EF1_Denominator = function( Pdata, Indiv_Wgts=TRUE,
                          fa=2e-06, fb=3.5, ma=2e-06, mb=3.5, ua=2e-06, ub=3.5 ) {

# For testing: fa=2e-06; fb=3.5; ma=2e-06; mb=3.5; ua=2e-06; ub=3.5


  # Clean up.  Muddled results if this function has been previously run.

  Pdata$Wt_Sampled_1   = NA
  Pdata$Wt_Sampled_2   = NA
  Pdata$LW_Calc_Wt     = NA
  Pdata$Wt_Sampled_3   = NA
  Pdata$Wt_Sampled     = NA
  Pdata$Wt_Method      = NA

  cat("\nGetting level-1 expansions\n\n")

  if ( Indiv_Wgts == TRUE ) {

    cat("\nIndividual weights will be generated from the following values:\n\n")
    cat("Females:", fa,fb, "Males:",  ma,mb, "Unknowns:",  ua,ub, "\n\n")

  } # End if

  # Assign 'state' if it's not already there.

  if (length(which(names(Pdata) == "state")) == 0) {

    cat("State variable is not assigned, getting state.\n\n")
    Pdata = getState(Pdata, CLEAN=T)

  }

  # Everything is calculated in terms of unique samples.

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  cat("\nGetting the summed weights of the sexed fish for each SAMPLE_NO\n\n")

  tows$Wt_Sampled_1 = tows$MALES_WGT + tows$FEMALES_WGT

  Pdata$Wt_Sampled_1 = tows$Wt_Sampled_1[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  cat("\nSumming the SPECIES_WEIGHTS for all clusters in a SAMPLE_NO\n\n")

  Pdata$SAMP_CLUST = paste(Pdata$SAMPLE_NO, Pdata$CLUSTER_NO, sep="_")
  uniqueClusters = Pdata[!duplicated(Pdata$SAMP_CLUST),]

  tmp = aggregate(uniqueClusters$SPECIES_WGT, list(uniqueClusters$SAMPLE_NO), sum, na.rm=T)
  names(tmp) = c("SAMPLE_NO", "wgt")

  # Might have 0 values that should actually be NA.  Aggregate doesn't generate NAs.

  tmp$wgt[tmp$wgt == 0] = NA

  tows$Wt_Sampled_2 = tmp$wgt[match(tows$SAMPLE_NO,tmp$SAMPLE_NO)]

  Pdata$Wt_Sampled_2 = tmp$wgt[match(Pdata$SAMPLE_NO,tmp$SAMPLE_NO)]

  # Only if there are individual weight factor and coefficients available

  if (Indiv_Wgts) {

    cat("\nCalculating individual weights from lengths.\n\n")

    ############################################################################
    #
    # Create a predicted fish weight based on sex and length
    # (use mm for fitted regression coefficients!)
    # these will be summed to give the sample weight
    #
    ############################################################################

    Pdata$LW_Calc_Wt = NA
    Pdata$LW_Calc_Wt[Pdata$SEX=="F"] = fa*Pdata$length[Pdata$SEX=="F"]^fb
    Pdata$LW_Calc_Wt[Pdata$SEX=="M"] = ma*Pdata$length[Pdata$SEX=="M"]^mb
    Pdata$LW_Calc_Wt[Pdata$SEX=="U"] = ua*Pdata$length[Pdata$SEX=="U"]^ub

    # Convert to pounds

    Pdata$LW_Calc_Wt = Pdata$LW_Calc_Wt * 0.00220462

    # Get the number of observed lengths and weights to use for each sample

    tmp = as.data.frame(table(Pdata$SAMPLE_NO))
    names(tmp) = c("SAMPLE_NO","numlens")

    tows$numlens = tmp$numlens[match(tows$SAMPLE_NO, tmp$SAMPLE_NO)]

    # Note:  if all else fails, fill 0 weights with the median in that sample.

    tmp_wt = aggregate(Pdata$LW_Calc_Wt, list(Pdata$SAMPLE_NO), sum, na.rm=T)
    med_wt = aggregate(Pdata$LW_Calc_Wt, list(Pdata$SAMPLE_NO), median, na.rm=T)
    tmp_wt = cbind(tmp_wt, med_wt[,2])
    names(tmp_wt) = c("SAMPLE_NO","Wt_Sampled_3","Median")

    tmp_wt$Wt_Sampled_3[tmp_wt$Wt_Sampled_3 == 0] = tmp_wt$Median[tmp_wt$Wt_Sampled_3 == 0]

    tows$Wt_Sampled_3 = tmp_wt$Wt_Sampled_3[match(tows$SAMPLE_NO, tmp_wt$SAMPLE_NO)]

    Pdata$Wt_Sampled_3 = tows$Wt_Sampled_3[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]
    Pdata$numlens = tows$numlens[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  } else {

    # Need for summary and boxplot

    Pdata$Wt_Sampled_3 = NA

  } # End if-else Indiv_Wgts


  ############################################################################
  #
  # Use calculated weights for Wt_Sampled.
  #
  ############################################################################

  tows$Wt_Sampled = tows$Wt_Sampled_1
  tows$Wt_Method = 1

  tows$Wt_Method[is.na(tows$Wt_Sampled)] = 2
  tows$Wt_Sampled[is.na(tows$Wt_Sampled)] = tows$Wt_Sampled_2[is.na(tows$Wt_Sampled)]

  if (Indiv_Wgts) {

    tows$Wt_Method[is.na(tows$Wt_Sampled)] = 3
    tows$Wt_Sampled[is.na(tows$Wt_Sampled)] = tows$Wt_Sampled_3[is.na(tows$Wt_Sampled)]

  } # End if

  tows$Wt_Method[is.na(tows$Wt_Sampled)] = NA

  # Match per-tow data to whole dataset.

  Pdata$Wt_Method = tows$Wt_Method[match(Pdata$SAMPLE_NO,tows$SAMPLE_NO)]
  Pdata$Wt_Sampled = tows$Wt_Sampled[match(Pdata$SAMPLE_NO,tows$SAMPLE_NO)]

  cat("\nDone calculating sample weights\n\n")

  # Summary and boxplot

  printemp = data.frame(cbind(Pdata$Wt_Sampled_1, Pdata$Wt_Sampled_2,
                              Pdata$Wt_Sampled_3, Pdata$Wt_Sampled))

  names(printemp) = c("M+F","SPECIES_WT","L-W","Final Wt_Sampled")

  print(summary(printemp))

  cat("\nWt_Methods:\n\n")

  print(summary(as.factor(Pdata$Wt_Method)))


  par(mfrow=c(2,2))


  boxplot(as.data.frame(printemp), names=c("M+F","SPECIES","L-W","Final Wt"),
          main = "Weight Sampled (Lbs) -- Denominator")

  NA_Wt_Sampled = Pdata[is.na(Pdata$Wt_Sampled),]
  nNA = nrow(NA_Wt_Sampled)

  if ( nNA > 0 ) {

    barplot(xtabs(NA_Wt_Sampled$FREQ ~ NA_Wt_Sampled$state + NA_Wt_Sampled$fishyr),
            col=rainbow(3),
            legend.text = T, xlab = "Year", ylab = "Samples",
            main="NA Sampled Wts -- Denominator")

  } else {

    cat("\nSample Wts found for all samples.\n\n")

  } # End if

  return(Pdata)

} # End EF1_Denominator


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


#############################################################################
#
# doSexRatio determines gender for unsexed fish depending on whether the
# Rvector is a single ratio, or the vector of ratios per length.
# If findRatio = TRUE, the ratios used are simply the line-by-line ratios
# of females:males.
#
# The input data expected are data that have already been aggregated by
# getComps.
#
# Bins are the length or age bins (for AGE comps) corresponding to the vector
# of ratios (unused for a single ratio).
#
# If NOT using a vector of lengths and corresponding ratios, then ratioU
# is the ratio applied to unsexed fish less than or equal to maxsizeU.
# This is usually 0.5, since these are fish so small that sexing them is
# probably not done correctly.
#
# GTsizeU is the size above which the ratio is assumed to be 1.0 (big mamas).
#
#############################################################################

doSexRatio = function( CompData, findRatio=FALSE, Rvector=0.5, Bins=NULL,
                       ratioU=.5, maxsizeU=0, GTsizeU=Inf ) {


  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!

  AGE_FLAG = FALSE

  if ( length(CompData$lengthcm) == 0 ) {

    index = which(names(CompData) == "age")
    names(CompData)[index] = "lengthcm"

    AGE_FLAG = TRUE

  } # End if

  # Fix arithmetic

  CompList = c("male","msamps","mtows","female","fsamps","ftows","unsexed","usamps","ONLY_U_TOWS","alltows")
  tmp = CompData[,CompList]
  tmp[is.na(tmp)] = 0
  CompData[,CompList] = tmp


  if ( maxsizeU != 0 ) {

    if ( length(Rvector) > 1 ) { stop("Rvector expected to be a single ratio") }

    # Create vectors from the appropriate values.

    Rvector = c(ratioU, Rvector, 1)
    Bins = c(0, maxsizeU, GTsizeU)

  } # End if

  if ( findRatio ) {

    cat("\nApplying per-stratum sex ratio.\n\n")

    getRatio = function( x, y ) {

      tmp = ( x / ( x + y ))
      tmp[x == 0 & y == 0] = 0.5
      tmp[x != 0 & y == 0] = 1

      return(tmp)

    } # End getRatio

    # KFJ: If bins are supplied, only use the ratio for those bins
    # that have a user supplied value of NA, this way a user can give
    # values for lengths below a certain size and then the calculated
    # ratio can be used for lengths greater than, or something along those
    # lines.

    # Also instead of using your own function you can use
    # calcRatio <- prop.table(CompData[, c("female", "male")], margin = 1)[, 1]

    # Though I think a good addition would be to look at neighboring bins
    # for a ratio rather than just assigning a value of 0.5 when there are
    # no observations in a given bin. Maybe even use a hierarchy of 2 nearest
    # bins, then same size but different year? Not sure on the best protocol here.
    # Could look to https://github.com/nwfsc-assess/nwfscSurvey/blob/master/R/SS3LF.fn.R
    # code for the sexRatio protocol to keep it consistent.

    calcRatio = getRatio(CompData$female, CompData$male)

    # If there are only unsexed in a stratum, need to replace NAs

    calcRatio[is.na(calcRatio)] = 0.5

    cat("\nSummary of sex ratios observed per stratum:\n\n")

    print(summary(calcRatio))

    # Now combine the calculated ratio with a user-supplied ratio

    if (length(Rvector) > 1) {

      Ratio <- Rvector[match(CompData$lengthcm, Bins)]
      Ratio[is.na(Ratio)] <- calcRatio[is.na(Ratio)]

    } else {

      # Use the calculated ratio for all bins
      Ratio <- calcRatio

    } # End if-else

    for ( i in 1:nrow(CompData) ) {

      CompData$female[i] = CompData$female[i] + CompData$unsexed[i] * Ratio[i]

      CompData$fsamps[i] = CompData$fsamps[i] + CompData$usamps[i] * Ratio[i]

      CompData$ftows[i] = CompData$ftows[i] + CompData$ONLY_U_TOWS[i] * Ratio[i]

      CompData$male[i] = CompData$male[i] + CompData$unsexed[i] * ( 1 - Ratio[i] )

      CompData$msamps[i] = CompData$msamps[i] + CompData$usamps[i] * ( 1 - Ratio[i] )

      CompData$mtows[i] = CompData$mtows[i] + CompData$ONLY_U_TOWS[i] * ( 1 - Ratio[i] )

    } # End for

    # If debugging:  CompData = cbind(CompData, Ratio)

    return(CompData)

  } # End if findRatio

  if ( length(Rvector) > 1 ) {

    if ( length(Bins) != length(Rvector) ) { stop("Rvector and Bins are not the same length") }

    tmp = paste(Rvector, collapse = ",")

    cat("\nApplying sex ratio:", tmp, "to numbers, samples and tows\n\n")

    # Recode lengths to correspond to bins given

    lens = findInterval(CompData$length, Bins, rightmost.closed=T)

    # Should be unnecessary

    if ( min(lens) == 0 ) {

      lens[lens == 0] = 1
      cat("Adjusting lengths below lbins up into first bin")

    } # End if

    for ( i in sort(unique(lens)) ) {

      CompData$female[lens == i] = CompData$female[lens == i] +
                                   CompData$unsexed[lens == i] *
                                   Rvector[i]

      CompData$fsamps[lens == i] = CompData$fsamps[lens == i] +
                                   CompData$usamps[lens == i] *
                                   Rvector[i]

      CompData$ftows[lens == i] = CompData$ftows[lens == i] +
                                  CompData$ONLY_U_TOWS[lens == i] *
                                  Rvector[i]

      CompData$male[lens == i] = CompData$male[lens == i] +
                                 CompData$unsexed[lens == i] *
                                 (1 - Rvector[i])

      CompData$msamps[lens == i] = CompData$msamps[lens == i] +
                                   CompData$usamps[lens == i] *
                                   (1 - Rvector[i])

      CompData$mtows[lens == i] = CompData$mtows[lens == i] +
                                  CompData$ONLY_U_TOWS[lens == i] *
                                  (1 - Rvector[i])

    } # End for

  } else {

    tmp = paste(Rvector, collapse = ",")
    cat("\nApplying sex ratio:", tmp, "to numbers, samples and tows\n\n")

    # apply a single ratio over all lengths

    CompData$female = CompData$female + Rvector * CompData$unsexed
    CompData$fsamps = CompData$fsamps + Rvector * CompData$usamps
    CompData$ftows = CompData$ftows + Rvector * CompData$ONLY_U_TOWS

    CompData$male = CompData$male + (1 - Rvector) * CompData$unsexed
    CompData$msamps = CompData$msamps + (1 - Rvector) * CompData$usamps
    CompData$mtows = CompData$mtows + (1 - Rvector) * CompData$ONLY_U_TOWS

  } # End if-else

  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!

  if (AGE_FLAG) {

    index = which(names(CompData) == "lengthcm")
    names(CompData)[index] = "age"

  } # End if

  cat("\nDone.\n\n")

  return(CompData)

} # End doSexRatio


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
