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
