##############################################################################
#
#  getState creates a state field that is developed from SOURCE_AGID, from
#  PSMFC_ARID, or from SAMPLE_AGENCY depending on the option selected.
#
#  If CLEAN is TRUE, records for which a state could not be assigned are
#  removed.
#
##############################################################################

getState = function ( Pdata, source="SOURCE_AGID", CLEAN=TRUE, keepPW=FALSE) {

  cat("\nGetting state information from", source, "\n\n")

  sources = c("SOURCE_AGID", "PSMFC_ARID", "SAMPLE_AGENCY" )

  if ( ! source %in% sources) {

    cat("\nError! Legitimate sources for getState are: ", sources, "\n")
    stop(" ")

  } # End if

  cat("\nOriginal data: \n\n")

  print(summary(as.factor(eval(parse(text=paste("Pdata$", source, sep=""))))))

  if (!CLEAN) {

    cat("\nGenerating data report only.  No data will be removed.\n\n")

    Original_data = Pdata

  }

  Pdata$state = Pdata[, source]

  if ( source == "PSMFC_ARID" ) {

    Pdata$state[Pdata$state %in% c("3A","3B","3S")] = "WA"
    Pdata$state[Pdata$state %in% c("2A","2B","2C")] = "OR"
    Pdata$state[Pdata$state %in% c("1A","1B","1C","CAL")] = "CA"

  } else {

    Pdata$state[Pdata$state == "C"] = "CA"
    Pdata$state[Pdata$state == "O"] = "OR"
    Pdata$state[Pdata$state == "W"] = "WA"
    Pdata$state[Pdata$state == "W"] = "WA"

  } # End if

  Pdata$state[Pdata$SOURCE_AGID == "CALCOM"] = "CA"

  # Remove stateless data

  states = c("OR","CA","WA")

  # PW is Pacific Whiting

  if ( keepPW ) { states = c(states, "PW") }

  nostate = sum(! Pdata$state %in% states )

  if (CLEAN) {

    Pdata = Pdata[Pdata$state %in% states,]

    cat("\n\nData retained: \n\n")

    print(summary(as.factor(eval(parse(text="Pdata$state")))))

    cat("\n\n", nostate, " records were removed because no state id could be assigned\n\n")

  } else {

    cat("There are ", nostate, " records for which no state id could be assigned\n")
    cat("\nReturning original data because CLEAN=FALSE\n\n")

  } # End if CLEAN

  return( Pdata )

} # end getState
