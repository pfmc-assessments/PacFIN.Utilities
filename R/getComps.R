##############################################################################
#
# getComps aggregates by length, by age, or by age-at-length according to the
# given stratification.
#
# Note that the aggregation is of the Pdata$Final_Sample_Size value, which should be
# set to the desired expansion, e.g. Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1
# or Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2
#
# The default stratification is by fleet, fishyr, and season.
# The lengthcm, age or both are appended depending on the "Comps" argument.
# The "strat" argument is prepended to this list.
#
##############################################################################

getComps = function( Pdata, strat=NULL, Comps="AAL" ) {

  # Check for expansion factor

  if ( length(Pdata$Final_Sample_Size) == 0 ) {

    cat("\nPlease make sure Pdata$Final_Sample_Size",
        "(the expansion factor)", "has a value\n\n")

    cat("Example:  Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1",
         "* Pdata$Expansion_Factor_2\n\n")

    return()

  } # End if

  # Set up stratification

  usualSuspects = c("fleet", "fishyr", "season")

  # Avoid duplication

  strat = strat[!strat %in% usualSuspects]

  if (Comps == "LENGTH" | Comps == "LEN") {

    TowStrat = usualSuspects
    usualSuspects = c(usualSuspects, "lengthcm")

  } else if (Comps == "AGE") {

    TowStrat = c(usualSuspects, "agemethod")
    usualSuspects = c(usualSuspects, "agemethod", "age")

  } else {

    TowStrat = c(usualSuspects, "agemethod", "age")
    usualSuspects = c(usualSuspects, "agemethod", "lengthcm", "age")

  } # End if-else-else

  Cstrat = c(strat, usualSuspects)

  cat("\nAggregating, stratification is by", paste(Cstrat, collapse=", "), "\n\n")
  flush.console()

  # Used to get the number of SAMPLE_NOs per aggregation

  lenique = function(x) { return(length(unique(x))) }

  # Set up flags for tows (SAMPLE_NOs) with only one gender present.
  # Need to retain this for subsequently assigning gender to the unsexed
  # and properly calculating sample sizes.

  sexedSamps = unique(Pdata$SAMPLE_NO[Pdata$SEX %in% c("F","M")])

  Usamps = unique(Pdata$SAMPLE_NO[Pdata$SEX == "U"])
  Uonly = Usamps[!Usamps %in% sexedSamps]

  Pdata$Uonly = NA
  Pdata$Uonly[Pdata$SAMPLE_NO %in% Uonly] = Pdata$SAMPLE_NO[Pdata$SAMPLE_NO %in% Uonly]

  # End setting up flag.

  # Aggregate all samples

  AllTows = aggregate(Pdata$SAMPLE_NO, Pdata[,TowStrat], lenique)

  cat("\nGetting Males\n\n")

  tmp = Pdata[Pdata$SEX == "M",]
  maleAgeComps = aggregate(tmp$Final_Sample_Size, tmp[,Cstrat], sum, na.rm=T)
  maleSamples = aggregate(tmp$FREQ, tmp[,Cstrat], sum, na.rm=T)
  maleTows = aggregate(tmp$SAMPLE_NO, tmp[,TowStrat], lenique)

  cat("\nGetting Females\n\n")

  tmp = Pdata[Pdata$SEX == "F",]
  femaleAgeComps = aggregate(tmp$Final_Sample_Size, tmp[,Cstrat], sum, na.rm=T)
  femaleSamples = aggregate(tmp$FREQ, tmp[,Cstrat], sum, na.rm=T)
  femaleTows = aggregate(tmp$SAMPLE_NO, tmp[,TowStrat], lenique)

  cat("\nGetting Unsexed\n\n")

  tmp = Pdata[Pdata$SEX == "U",]
  unSexedAgeComps = aggregate(tmp$Final_Sample_Size, tmp[,Cstrat], sum, na.rm=T)
  unSamples = aggregate(tmp$FREQ, tmp[,Cstrat], sum, na.rm=T)
  unTows = aggregate(tmp$SAMPLE_NO, tmp[,TowStrat], lenique)

  # Used to partition tows in doSexRatio

  uONLYTows = aggregate(tmp$Uonly, tmp[,TowStrat], lenique)

  names(maleAgeComps) = c(Cstrat, "male")
  names(femaleAgeComps) = c(Cstrat, "female")
  names(unSexedAgeComps) = c(Cstrat, "unsexed")

  names(maleSamples) = c(Cstrat, "msamps")
  names(femaleSamples) = c(Cstrat, "fsamps")
  names(unSamples) = c(Cstrat, "usamps")

  names(maleTows) = c(TowStrat, "mtows")
  names(femaleTows) = c(TowStrat, "ftows")
  names(unTows) = c(TowStrat, "utows")
  names(uONLYTows) = c(TowStrat, "ONLY_U_TOWS")
  names(AllTows) = c(TowStrat, "alltows")

  # Add tows and samples to the Comps

  cat("Getting tows and samples\n\n")

  maleAgeComps = merge(maleAgeComps, maleSamples, by=Cstrat, all=T)
  maleAgeComps = merge(maleAgeComps, maleTows, by=TowStrat, all=T)

  femaleAgeComps = merge(femaleAgeComps, femaleSamples, by=Cstrat, all=T)
  femaleAgeComps = merge(femaleAgeComps, femaleTows, by=TowStrat, all=T)

  unSexedAgeComps = merge(unSexedAgeComps, unSamples, by=Cstrat, all=T)
  unSexedAgeComps = merge(unSexedAgeComps, unTows, by=TowStrat, all=T)
  unSexedAgeComps = merge(unSexedAgeComps, uONLYTows, by=TowStrat, all=T)

  ageComps = merge(maleAgeComps, femaleAgeComps, by=Cstrat, all=T)
  ageComps = merge(ageComps, unSexedAgeComps, by=Cstrat, all=T)
  ageComps = merge(ageComps, AllTows, by=TowStrat, all=T)

  # Needed for arithmetic.

  ageComps[is.na(ageComps)] = 0

  invisible(ageComps)

} # End function getComps
