#' Expand PacFIN Samples to Catch
#'
#' The second-stage expansion calculates the
#' per-year, per-trip, per-stratum total catch
#' divided by the sampled catch and appends it to the input data as
#' \code{Expansion_Factor_2}.
#'
#' @export
#'
#' @author Andi Stephens
#'
#' @seealso \code{\link{Stratify}}
#'
#' @param Pdata A cleaned PacFIN dataframe with column "stratification" appended.
#' @param Catch A dataframe of catch data, in pounds or in metric tonnes.
#' @param Units The units of the \code{Catch} data frame, see
#' \code{measurements::conv_unit_options[["mass"]]}
#' for options. Typical units are metric tonnes (e.g., \code{"metric_ton"})
#' because that is the unit used in Stock Synthesis, but
#' expansions are done in pounds because fish weights are in pounds.
#' Thus, catches also need to be in pounds and will be converted as such.
#' @param Convert A deprecated argument that is now set to \code{NULL}.
#' Previously, it was a logical that defined if the Catch should be converted from
#' metric tonnes to pounds, where \code{TRUE} is now the same as setting
#' \code{Units = "MT"} and \code{FALSE}, which was the default, would be
#' \code{Units = "LB"}. Normally, one would have their catch in metric tonnes,
#' i.e., \code{Convert = TRUE} or \code{Units = "MT"},
#' such that it can be used within Stock Synthesis.
#' todo: remove this input argument
#' @template maxExp
#' 
#' @details
#' The workflow requires that \code{getExpansion_2} is run after
#' \code{\link{getExpansion_1}()} using the same data frame.
#' The input PacFIN dataset must contain a column called \code{stratification},
#' a character-valued user-designed stratification of the data whose values match the
#' column-names in the Catch dataset.
#' 
#' For example, if you've set \code{stratification} so that:
#' 
#' \code{   Pdata$stratification[ Pdata$geargroup == "TWL" & Pdata$state == CA ] = "S_TWL"}
#' \code{   Pdata$stratification[ Pdata$geargroup == "TWL" & Pdata$state != CA ] = "N_TWL"}
#' 
#' then the columns in Catch must be named "Year", "S_TWL" and "N_TWL"
#' 
#' or if you used:
#'
#' \code{   Pdata$stratification[ Pdata$geargroup == "TWL" & Pdata$season == 1 ] = "WINTER_TWL"} 
#' \code{   Pdata$stratification[ Pdata$geargroup == "TWL" & Pdata$season == 2 ] = "SUMMER_TWL"}
#' 
#' then the columns in Catch must be named "Year", "WINTER_TWL", and "SUMMER_TWL".
#' 
#' You can use as many levels of stratification in your data and catch as you want,
#' as long as you encode those levels in \code{stratification} and in the column names
#' in your data.  \strong{Year} is automatically included as a level of stratification.
#'
#' @template secExpansion
#'
#' @return
#' The input PacFIN dataset, with column \code{Expansion_Factor_2} appended.
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#' 
#' 
#' 
# Calculate the stratified sampled biomass, All_Trips_Sampled_Lbs by summing
# Trip_Sampled_Lbs. Calculate the stratified catch by summing MT * 2204.  The
# per-trip, per-stratum Expansion_Factor_2 is the catch / sampled catch.
#
#############################################################################

getExpansion_2 <- function(Pdata, Catch,
  Units = c("MT", "LB"), Convert = NULL, maxExp = 0.95) {

  # Check Unit input
  Units <- match.arg(Units, several.ok = FALSE,
    choices = c(measurements::conv_unit_options[["mass"]], "MT", "LB"))
  Units <- switch(Units,
    MT = "metric_ton",
    LB = "lbs",
    Units)

  # Check and stop if Convert input is used since it is not deprecated
  if(!is.null(Convert)) {
    stop("Convert is deprecated.",
      "Please specify the units of Catch via the Unit input (MT or LB).\n",
      paste(measurements::conv_unit_options[["mass"]], collapse = ", "), "\n\n")
  }

  # Start clean

  Pdata$Expansion_Factor_2 = NA

  if (length(Pdata$Trip_Sampled_Lbs) == 0) {

    stop("Please run getExpansion_1 first\n\n")

  } # End if


  # Pdata must have a "stratification" column encoding gear.  
  
  if (length(Pdata$stratification) == 0) {

    stop("Input data must have stratification encoded in column 'stratification'\n\n")

  } # End if

  ############################################################################
  # Check catch against Pdata.
  ############################################################################

  # Catch is expected to have columns named for state and gear, as well as a "Year" column.

  Catchgears = sort(names(Catch)[2:length(names(Catch))])

  Pstrat = sort(unique(Pdata$stratification))

  if ( !identical(Pstrat,Catchgears) ) {

    cat("Error:  mismatch between dataset and catch.\n\n")

    cat("Catch: ", Catchgears, "\n\n")
    cat("Data:  ", Pstrat, "\n\n")

    if (sum(Pstrat %in% Catchgears) == 0) {
      stop("\nNo bds stratifications,\n",
        paste(Pstrat, collapse = ", "), "\n",
        "were found in catch stratifications,\n",
        paste(Catchgears, collapse = ", "))
    } else {
      Pdata <- Pdata[Pdata[, "stratification"] %in% colnames(Catch), ]
      Catch <- Catch[, c("Year", unique(Pdata[, "stratification"]))]
      Catchgears = sort(names(Catch)[2:length(names(Catch))])
      Pstrat = sort(unique(Pdata$stratification))
      message("The following were truncated by these stratifications:")
      message("Catch: ", paste(Catchgears, collapse = ", "))
      message("Pdata: ", paste(Pstrat, collapse = ", "))
    }

  } # End if

  # Get summed sampled lbs per individual sample (tow).

  tows <- Pdata[!duplicated(Pdata$SAMPLE_NO),]

  # Get the totals by year and stratification

  strat = c("fishyr", "stratification")

  SumSampled <- aggregate(tows$Trip_Sampled_Lbs, 
    tows[,strat], sum, na.rm=T)

  names(SumSampled)[3] = "Sum_Sampled_Lbs"

  tows$Sum_Sampled_Lbs <- find.matching.rows(
    tows, SumSampled, strat, strat,
    "Sum_Sampled_Lbs")[[1]]

  # Convert Catch to lbs.
  Catch[ , 2:ncol(Catch)] <- measurements::conv_unit(to = "lbs",
    x = Catch[ , 2:ncol(Catch)], from = Units)

  # Matching is on Year == fishyr.
  # Pdata$catch col gets the matched Catch.

  tows$catch <- tows$state.gear <- NULL

  for ( sg in 2:ncol(Catch) ) {

    state.gear = names(Catch)[sg]

    for ( yr in Catch$Year ) {

      tows$state.gear = state.gear
      tows$catch[tows$fishyr == yr & tows$stratification == state.gear ] = 
        Catch[Catch$Year == yr, sg]

      cat(".")

    } # End for Year

    cat("Assigned catch for ", state.gear, "\n")

  } # End for Catch

  NoCatchYr = tows$fishyr[is.na(tows$catch)]
  NoCatchSG = tows$state.gear[is.na(tows$catch)]
  NoCatch = cbind(NoCatchYr, NoCatchSG)
  NoCatch = NoCatch[! duplicated(NoCatch),]

  # KFJ - only print if an issue
  # Andi -- thanks!

  if (length(NoCatch) > 0) {

    cat("\nNo Catch was found for these combinations:\n\n")
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

  cat("\nSummary of Expansion_Factor_2\n\n")
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
  Pdata[, "Final_Sample_Size"] <- capValues(Pdata$Expansion_Factor_1_L * Pdata$Expansion_Factor_2)

  cat("Summary of Expansion_Factor_2\n\n")
  print(summary(Pdata$Expansion_Factor_2))

  boxplot(Pdata$Expansion_Factor_2 ~ Pdata$fishyr, main="Expansion_Factor_2")

  cat("\nRemember to set (or reset) Pdata$Final_Sample_Size\n\n")


  invisible(Pdata)

} # End function getExpansion_2
