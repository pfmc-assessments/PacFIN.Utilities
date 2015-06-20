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
