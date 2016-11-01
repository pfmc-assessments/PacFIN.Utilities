#' Complete a first level expansion for composition data where the unsampled
#' fish in a tow are accounted for.
#'
#' @details \code{getExpansion_1} calls \code{\link{EF1_Numerator}} and
#'   \code{\link{EF1_Denominator}} (i.e., the weight of sampled fish and
#'   the weight of all fish of the respective species in the tow) and returns
#'   their ratio.
#' @template Pdata
#' @param maxExp The maximum expansion factor (either a number or a quantile).
#' @template Indiv_Wgts
#' @template weightlengthparams
#' @template verbose
#' @template plot
#' @return A \code{data.frame} where all of the original columns in
#'   \code{Pdata} remain unaltered but additional columns are added.

getExpansion_1 = function(Pdata, maxExp = 0.95, Indiv_Wgts = TRUE,
  fa = 2e-06, fb = 3.5, ma = 2e-06, mb = 3.5, ua = 2e-06, ub = 3.5, verbose = TRUE,
  plot = FALSE) {

  # Get the Wt_Sampled
  if (is.character(plot)) {
    fn <- gsub(".png", "", plot)
    plot.denom <- paste0(fn, "_denom.png")
  } else {
    if (plot == TRUE) {
      plot.denom <- TRUE
      dev.new()
    } else plot.denom <- FALSE
  }

  Pdata = EF1_Denominator(Pdata, Indiv_Wgts,
    fa, fb, ma, mb, ua, ub, verbose = verbose, plot = plot.denom)

  # Get Trip_Sampled_Lbs
  if (is.character(plot)) {
    fn <- gsub(".png", "", plot)
    plot.num <- paste0(fn, "_numer.png")
  } else {
    if (plot == TRUE) {
      plot.num <- TRUE
      dev.new()
    } else plot.num <- FALSE
  }
  Pdata = EF1_Numerator(Pdata, verbose = verbose, plot = plot.num)

  # Expansion_Factor_1

  Pdata$Expansion_Factor_1 = Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled

  Pdata$Expansion_Factor_1[Pdata$Expansion_Factor_1 < 1] = 1



  NA_EF1 = Pdata[is.na(Pdata$Expansion_Factor_1),]
  nNA = nrow(NA_EF1)

  if (verbose) {
    cat("\n", nNA, "NA Expansion_Factor_1 values replaced by 1.\n\n")
  }

  # Now replace NAs with 1.
  Pdata$Expansion_Factor_1[is.na(Pdata$Expansion_Factor_1)] = 1

  Pdata$Expansion_Factor_1 = capValues(Pdata$Expansion_Factor_1, maxExp)

  if (verbose) {
    cat("\nCapping Expansion_Factor_1 at ", maxExp, "\n\n")
    print(summary(Pdata$Expansion_Factor_1))
  }

  # KFJ(2015-06-16): Generate plots and save them to the disk if specified.
  if (plot != FALSE){
    if (is.character(plot)) png(plot) else dev.new()
    if (nNA > 0) {
      # Plot NA values by year and state.  Early years data or CALCOM data?
      par(mfrow = c(2, 1), mar = c(0, 3, 0, 0), oma = c(4, 1, 3, 0),
        mgp = c(2.0, 0.5, 0))
      allyears <- seq(min(Pdata$fishyr), max(Pdata$fishyr), by = 1)
      vals <- matrix(0, nrow = length(unique(NA_EF1$state)), ncol = length(allyears))
      rownames(vals) <- unique(NA_EF1$state)
      colnames(vals) <- allyears
      bad <- as.matrix(table(NA_EF1$state, NA_EF1$fishyr))
      vals[, colnames(vals) %in% colnames(bad)] <- bad
      barplot(vals,
        col = rainbow(length(unique(NA_EF1$state))),
        legend.text = TRUE, xlab = "", xaxt = "n",
        ylab = "Replace NA in Exp_1 with 1",
        args.legend = list(bty = "n"))
    } else {
      par(mgp = c(2, 0.5, 0), mar = c(1.5, 3, 1, 0), mfrow = c(1, 1))
    } # End if

    boxplot(Pdata$Expansion_Factor_1 ~ Pdata$fishyr, ylab ="Expansion_Factor_1",
      xlab = "", frame.plot = FALSE)
    mtext(side = 1, outer = TRUE, "Year", line = 2)
    if (is.character(plot)) dev.off()
  }
  return(Pdata)

} # End function getExpansion_1
