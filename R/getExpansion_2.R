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
