###########################################################################
#
#' Aggregate composition data by length, age, or age-at-length according to the
#' given stratification.
#' 
#' \subsection{\code{\link{Workflow}}}{
#' \code{getComps} is run subsequently to \code{\link{getExpansion_2}}.
#' }
#' 
#' @export
#'
#' @details The aggregation is of the \code{Pdata$Final_Sample_Size} column value,
#'   which should be set to the desired expansion:
#'   
#'   \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1} 
#'   
#'   or
#'   
#'   \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2}
#'   
#'   The default stratification is by fleet, fishyr, and season.
#'   Columns \code{lengthcm}, \code{age} or both are added
#'   depending on the \code{Comps} argument.
#'   
#' @template Pdata
#' @param strat A character value or vector of character values, of which are
#'   prepended to \code{defaults}. For instance if you wish to add ageing method
#'   as a stratification use \code{strat = 'agemethod'}.
#' @param Comps The type of composition data to create. Options are length
#'   (\code{'LEN'}, age (\code{'AGE'}), or conditional age-at-length (\code{'AAL'}).
#' @param defaults The default stratification columns
#'   which will typically be left at their default value of
#'   \code{c('fleet', 'fishyr', 'season')}.
#' @template verbose
#' @return A dataframe with composition data specific to the type specified
#'   in \code{Comps} for males, females, and unsexed records.
#' @author Andi Stephens, Kelli Faye Johnson
#' 
############################################################################


getComps = function( Pdata, strat = NULL, Comps = "AAL",
  defaults = c("fleet", "fishyr", "season"), verbose = TRUE) {

  # Check for expansion factor

  if (length(Pdata$Final_Sample_Size) == 0) {
    
    stop(paste("\ngetComps relies on the column labeled 'Final_Sample_Size'\n",
      "please make sure this column (the expansion factor) has a value.\n\n",
      "Example: Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1",
      "* Pdata$Expansion_Factor_2\n"))
    
  } # End if

  # Set up stratification

  usualSuspects <- defaults

  # Avoid duplication

  strat = strat[!strat %in% usualSuspects]

  if (Comps == "LENGTH" | Comps == "LEN") {

    TowStrat = usualSuspects
    usualSuspects = c(usualSuspects, "lengthcm")

  } else if (Comps == "AGE") {

    TowStrat = usualSuspects
    usualSuspects = c(usualSuspects, "age")

  } else {

    TowStrat = c(usualSuspects, "age")
    usualSuspects = c(usualSuspects, "lengthcm", "age")

  } # End if-else-else

  Cstrat = c(strat, usualSuspects)

  if (verbose) {
    cat("\nAggregating, stratification is by", paste(Cstrat, collapse=", "), "\n\n")
    flush.console()
  }
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

  tmp = Pdata[Pdata$SEX == "M",]
  maleAgeComps = aggregate(tmp$Final_Sample_Size, tmp[,Cstrat], sum, na.rm=T)
  maleSamples = aggregate(tmp$FREQ, tmp[,Cstrat], sum, na.rm=T)
  maleTows = aggregate(tmp$SAMPLE_NO, tmp[,TowStrat], lenique)

  tmp = Pdata[Pdata$SEX == "F",]
  femaleAgeComps = aggregate(tmp$Final_Sample_Size, tmp[,Cstrat], sum, na.rm=T)
  femaleSamples = aggregate(tmp$FREQ, tmp[,Cstrat], sum, na.rm=T)
  femaleTows = aggregate(tmp$SAMPLE_NO, tmp[,TowStrat], lenique)

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

  # KFJ(2015-06-16): Do the above in few lines
  # ageComps <- merge(by = c(TowStrat, "SEX"), all = TRUE,
  #   aggregate(Pdata[, c("Final_Sample_Size", "FREQ")], Pdata[, c(Cstrat, "SEX")],
  #     sum, na.rm = TRUE),
  #   aggregate(Pdata[, c("SAMPLE_NO", "Uonly")], Pdata[, c(TowStrat, "SEX")],
  #     lenique))
  # ageComps <- reshape(ageComps, timevar = "SEX", idvar = Cstrat, direction = "wide")
  # ageComps <- merge(ageComps, aggregate(Pdata$SAMPLE_NO, Pdata[, TowStrat], lenique),
  #   by = TowStrat, all.x = TRUE)
  # ageComps <- ageComps[, -grep("Uonly.F|Uonly.M", colnames(ageComps))]
  # colnames(ageComps) <- c(Cstrat, "female", "fsamps", "ftows",
  #   "male", "msamps", "mtows", "unsexed", "usamps", "utows", "ONLY_U_TOWS", "alltows")
  # ageComps[is.na(ageComps)] <- 0

  invisible(ageComps)

} # End function getComps
