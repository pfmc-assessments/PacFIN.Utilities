#' First-Stage Expansion for Composition Data
#'
#' First-stage expansions account for unsampled fish in the smallest
#' measured unit. Where, in PacFIN data, the smallest unit is that
#' which is available to the port or dockside sampler, so more than
#' likely a trip rather than a tow as would be with survey data.
#'
#' @export
#' @seealso Called within this function are
#' \code{\link{EF1_Numerator}} \code{\link{EF1_Denominator}}.
#' Called after this function should be \code{\link{getExpansion_2}}.
#'
#' @details
#' The workflow is to run \code{getExpansion_1} after \code{\link{cleanPacFIN}},
#' which assures that all of the necessary columns are available and that the
#' data are in the correct units.
#' \code{getExpansion_1} calls
#' \code{\link{EF1_Numerator}} and \code{\link{EF1_Denominator}}
#' (i.e., the weight of sampled fish and
#' the weight of all fish of the respective species in the tow)
#' and returns their ratio.
#'
#' @template secExpansion
#'
#' @template Pdata
#' @template maxExp
#' @param Exp_WA Default FALSE.  If TRUE, expand the WA samples.
#' @template weightlengthparams
#' @template verbose
#' @template plot
#'
#' @return A \code{data.frame} where all of the original columns in
#' \code{Pdata} remain unaltered but additional columns are added.
#' In particular columns starting with
#' Expansion_Factor_1 are available for setting the Final_Expansion_Factor.
#'
getExpansion_1 <- function(Pdata, maxExp = 0.95,
  Exp_WA = TRUE,
  fa = NA, fb = NA, ma = NA, mb = NA, ua = NA, ub = NA,
  verbose = FALSE, plot = FALSE) {

  # Calculate length-weight relationship
  if (all(mapply(is.na, c(fa, ma, ua)))) {
    pars <- getWLpars(data = Pdata, verbose = FALSE)
    fa <- ifelse(is.na(fa), pars["females", "A"], fa)
    fb <- ifelse(is.na(fb), pars["females", "B"], fb)
    ma <- ifelse(is.na(ma), pars["males", "A"], ma)
    mb <- ifelse(is.na(mb), pars["males", "B"], mb)
    ua <- ifelse(is.na(ua), pars["all", "A"], ua)
    ub <- ifelse(is.na(ub), pars["all", "B"], ub)
  }

  if (is.character(plot)) {
    plot.denom <- ifelse(grepl("png", tools::file_ext(plot)),
       dirname(plot), plot)
  } else {
    if (plot == TRUE) {
      plot.denom <- TRUE
    } else plot.denom <- FALSE
  }

  Pdata <- EF1_Denominator(Pdata,
    fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub,
    verbose = verbose, plot = plot.denom)

  # Get Trip_Sampled_Lbs
  if (is.character(plot)) {
    fn <- gsub(".png", "", plot)
    plot.num <- file.path(ifelse(!grepl("png", tools::file_ext(plot)),
      plot, dirname(plot)), "PacFIN_exp1_numer.png")
  } else {
    if (plot == TRUE) {
      plot.num <- TRUE
      grDevices::dev.new()
    } else plot.num <- FALSE
  }
  Pdata <- EF1_Numerator(Pdata, verbose = verbose, plot = plot.num)

  # Expansion_Factor_1

  Pdata$Expansion_Factor_1_L <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_L
  Pdata$Expansion_Factor_1_A <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_A

  Pdata$Expansion_Factor_1_L[Pdata$Expansion_Factor_1_L < 1] <- 1
  Pdata$Expansion_Factor_1_A[Pdata$Expansion_Factor_1_A < 1] <- 1

  # In most cases, WA data can't be expanded.
  if (Exp_WA != TRUE) {
    Pdata$Expansion_Factor_1_L[Pdata$state == "WA"] <- 1
    Pdata$Expansion_Factor_1_A[Pdata$state == "WA"] <- 1
    cat("\n\nWA expansions set to 1. Fish tickets do not represent whole trips in WA.\n\n")
  } # End if

  NA_EF1 <- Pdata[is.na(Pdata$Expansion_Factor_1_L),]
  nNA <- NROW(NA_EF1)

  if (verbose) {
    cat("\n", nNA, "NA Expansion_Factor_1 values replaced by 1.\n\n")
  }

  # Now replace NAs with 1.
  Pdata$Expansion_Factor_1_L[is.na(Pdata$Expansion_Factor_1_L)] <- 1
  Pdata$Expansion_Factor_1_A[is.na(Pdata$Expansion_Factor_1_A)] <- 1

  Pdata$Expansion_Factor_1_L <- capValues(Pdata$Expansion_Factor_1_L, maxExp)
  Pdata$Expansion_Factor_1_A <- capValues(Pdata$Expansion_Factor_1_A, maxExp)

  if (verbose) {
    cat("\nCapping Expansion_Factor_1 at ", maxExp, "\n\n")
    print(summary(Pdata$Expansion_Factor_1_L))
  }

  # Generate plots and save them to the disk if specified.
  if (plot != FALSE){

    if (is.character(plot)) {
      grDevices::png(file.path(ifelse(!grepl("png", tools::file_ext(plot)),
        plot, dirname(plot)), "PacFIN_exp1.png"))
    } else {
      grDevices::dev.new()
    }

    if (nNA > 0) {
      # Plot NA values by year and state.  Early years data or CALCOM data?
      graphics::par(mfrow = c(2, 1), mar = c(0, 3, 0, 0), oma = c(4, 1, 3, 0),
        mgp = c(2.0, 0.5, 0))

      allyears <- seq(min(Pdata$fishyr), max(Pdata$fishyr), by = 1)
      vals <- matrix(0,
        nrow = length(unique(NA_EF1$state)),
        ncol = length(allyears))
      rownames(vals) <- unique(NA_EF1$state)
      colnames(vals) <- allyears
      bad <- as.matrix(table(NA_EF1$state, NA_EF1$fishyr))
      vals[, colnames(vals) %in% colnames(bad)] <- bad

      graphics::barplot(vals,
        col = grDevices::rainbow(length(unique(NA_EF1$state))),
        legend.text = TRUE, xlab = "", xaxt = "n",
        ylab = "Replace NA in Exp_1 with 1",
        args.legend = list(bty = "n"))
    } else {
      graphics::par(
        mgp = c(2, 0.5, 0),
        mar = c(1.5, 3, 1, 0),
        mfrow = c(1, 1))
    } # End if

    graphics::boxplot(Pdata$Expansion_Factor_1_L ~ Pdata$fishyr,
      ylab ="Expansion_Factor_1_L",
      xlab = "", frame.plot = FALSE)
    
    graphics::mtext(side = 1, outer = TRUE, "Year", line = 2)
    
    if (is.character(plot)) {
      grDevices::dev.off()
    }
  }
  return(Pdata)

} # End function getExpansion_1
