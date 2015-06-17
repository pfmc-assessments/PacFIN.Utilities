#' Complete a first level expansion for composition data where the unsampled
#' fish in a tow are accounted for.
#'
#' @details \code{getExpansion_1} calls \link{\code{EF1_Numerator}} and
#'   \link{\code{EF1_Denominator}} (i.e., the weight of sampled fish and
#'   the weight of all fish of the respective species in the tow) and returns
#'   their ratio.
#' @template Pdata
#' @param maxExp The maximum expansion factor (either a number or a quantile).
#' @param Indiv_Wgts
#' @template weightlengthparams
#' @return A \code{data.frame} where all of the original columns in
#'   \code{Pdata} remain unaltered but additional columns are added.

getExpansion_1 = function( Pdata, maxExp=0.95, Indiv_Wgts=TRUE,
                           fa=2e-06, fb=3.5, ma=2e-06, mb=3.5, ua=2e-06, ub=3.5 ) {

  # Get the Wt_Sampled

  Pdata = EF1_Denominator( Pdata, Indiv_Wgts,
                           fa, fb, ma, mb, ua, ub )

  # Get Trip_Sampled_Lbs

  Pdata = EF1_Numerator( Pdata )

  # Expansion_Factor_1

  Pdata$Expansion_Factor_1 = Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled

  Pdata$Expansion_Factor_1[Pdata$Expansion_Factor_1 < 1] = 1

  # Plot NA values by year and state.  Early years data or CALCOM data?

  NA_EF1 = Pdata[is.na(Pdata$Expansion_Factor_1),]
  nNA = nrow(NA_EF1)

  cat("\n", nNA, "NA Expansion_Factor_1 values replaced by 1.\n\n")

  if (nNA > 0) {

    barplot(xtabs(NA_EF1$FREQ ~ NA_EF1$state + NA_EF1$fishyr),
            col=rainbow(3),
            legend.text = T, xlab = "Year", ylab = "Samples",
            main="NA Expansion_Factor_1 replaced by 1")

  } # End if

  # Now replace NAs with 1.

  Pdata$Expansion_Factor_1[is.na(Pdata$Expansion_Factor_1)] = 1

  cat("\nCapping Expansion_Factor_1 at ", maxExp, "\n\n")

  Pdata$Expansion_Factor_1 = capValues(Pdata$Expansion_Factor_1, maxExp)

  cat("\nExpansion Factor 1:\n\n")

  print(summary(Pdata$Expansion_Factor_1))

  boxplot(Pdata$Expansion_Factor_1 ~ Pdata$fishyr, main="Expansion_Factor_1")

  return(Pdata)

} # End function getExpansion_1
