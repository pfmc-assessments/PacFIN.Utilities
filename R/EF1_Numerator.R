#' Calculate the numerator for the first level expansion factor.
#'
#' @details \code{Trip_Sampled_Lbs} is calculated differently for each state:
#' \itemize{
#' \item {California}{ = \code{Pdata$Species_Percent_Sampled} *
#'   \code{Pdata$TOTAL_WGT};}
#' \item {Oregon}{ = \code{Pdata$EXP_WT}.  Where missing,
#'   use \code{Pdata$Species_Percent_Sampled}, as for CA;}
#' \item {Washington}{ = \code{Pdata$RWT_LBS}, \code{Pdata$TOTAL_WGT},
#'   median(RWT_LBS), or median(\code{Pdata$TOTAL_WGT});}
#' \item {if all else fails}{ = use per-year, state-specific medians.}
#' }
#' @return A \code{Pdata} with additional columns, where
#'   \code{Species_Percent_Sampled} is the percentage of this species in the samples;
#'   \code{Use_acs} is \code{all_cluster_sum} with NAs replaced by year-specific,
#'   state-specific medians;
#'   \code{Trip_Sampled_Lbs} is the sample weight in pounds;
#'
#' @template Pdata
#' @template verbose
#' @template plot
#' @author Andi Stephens

EF1_Numerator = function(Pdata, verbose = FALSE, plot = FALSE) {

  # Start clean

  Pdata$Use_acs        = NA
  Pdata$Species_Percent_Sampled = NA
  Pdata$Trip_Sampled_Lbs = NA

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  ############################################################################
  #
  # First the all_cluster_sums, replacing NA with medians.
  #
  #############################################################################
  tows$Use_acs = tows$all_cluster_sum

  tows$Use_acs[tows$Use_acs == 0] = NA

  # Use median from the state and year where total landed weights are missing

  # KFJ(2015-06-17): Should use state x year x gear then most recent year
  # if not available.
  median_use_acs = aggregate(tows$Use_acs, list(tows$state, tows$fishyr), median, na.rm=T)
  names(median_use_acs) = c("state","fishyr","Median")

  # Might be all NA for one state, get the overall median

  overall_median = aggregate(tows$Use_acs, list(tows$fishyr), median, na.rm=T)
  names(overall_median) = c("fishyr","Median")

  # Get row-by-row alignment with tows for each median.
  # Note that find.matching.rows returns a list; [[1]] is the values.

  Use_acs_medians = find.matching.rows(tows, median_use_acs,
                    c("state","fishyr"), c("state", "fishyr"), "Median")[[1]]

  Annual_medians = find.matching.rows(tows, overall_median,
                                      c("fishyr"), c("fishyr"), "Median")[[1]]

  # First fill NA with state/fishyr median, then fishyr median.

  tows$Use_acs[is.na(tows$Use_acs)] = Use_acs_medians[is.na(tows$Use_acs)]
  tows$Use_acs[is.na(tows$Use_acs)] = Annual_medians[is.na(tows$Use_acs)]

  # Save to full dataset.

  Pdata$Use_acs = tows$Use_acs[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  ############################################################################
  #
  # Calculate Species_Percent_Sampled.  (Actually, fraction sampled).
  #
  ############################################################################

  # For CA and OR:

  tows$Species_Percent_Sampled = tows$Wt_Sampled/tows$Use_acs
  tows$Use_Percent = tows$Species_Percent_Sampled * tows$TOTAL_WGT

  ############################################################################
  #
  # Get total weight per SAMPLE_NO, calculated differently for each state.
  # Replace NAs with state/fishyr specific medians.
  #
  ############################################################################

  # Default

  tows$Trip_Sampled_Lbs = tows$TOTAL_WGT

  # California

  tows$Trip_Sampled_Lbs[tows$state=="CA"] = tows$Use_Percent[tows$state=="CA"]

  # Oregon uses exp_wt by preference.
  # OR data is always missing total weights between 1965-1970
  # so use Species_Percent_Sampled, as for CA.

  indices = which(tows$state == "OR" & tows$exp_wt > 0)
  tows$Trip_Sampled_Lbs[indices] = tows$exp_wt[indices]

  tows$Trip_Sampled_Lbs[tows$Trip_Sampled_Lbs == 0] = NA

  indices = which(tows$state=="OR" & is.na(tows$Trip_Sampled_Lbs))
  tows$Trip_Sampled_Lbs[indices] = tows$Use_Percent[indices]

  # Washinton

  # Is RWT_LBS available?

  if (length(tows$RWT_LBS) != 0) {

    tows$Trip_Sampled_Lbs[tows$state=="WA"] = tows$RWT_LBS[tows$state=="WA"]

  } else {
    if (verbose) {
      cat("\nWarning:  data does not contain column RWT_LBS required for WA data\n\n")
    }

    Pdata$RWT_LBS = NA
    tows$RWT_LBS = NA

  } # End if

  indices = which(tows$state=="WA" & is.na(tows$Trip_Sampled_Lbs))
  tows$Trip_Sampled_Lbs[indices] = tows$TOTAL_WGT[indices]

  # Calculate state and fishyr specific medians.

  med_Trip_Sampled_Lbs = aggregate(tows$Trip_Sampled_Lbs,
                                   list(tows$state, tows$fishyr),
                                   median, na.rm=T)

  med_RWT_LBS          = aggregate(tows$RWT_LBS,
                                   list(tows$state, tows$fishyr),
                                   median, na.rm=T)

  med_TOTAL_WGT        = aggregate(tows$RWT_LBS,
                                   list(tows$state, tows$fishyr),
                                   median, na.rm=T)

  names(med_Trip_Sampled_Lbs) = c("state", "fishyr", "Median")
  names(med_RWT_LBS)          = c("state", "fishyr", "Median")
  names(med_TOTAL_WGT)        = c("state", "fishyr", "Median")

  # Get row-by-row alignment with tows for each median.

  Trip_Sampled_medians = find.matching.rows(tows, med_Trip_Sampled_Lbs,
                         c("state","fishyr"), c("state", "fishyr"), "Median")[[1]]

  RWT_LBS_medians      = find.matching.rows(tows, med_RWT_LBS,
                         c("state","fishyr"), c("state", "fishyr"), "Median")[[1]]

  TOTAL_WGT_medians    = find.matching.rows(tows, med_TOTAL_WGT,
                         c("state","fishyr"), c("state", "fishyr"), "Median")[[1]]

  # Fill CA and OR annual median Trip_Sampled_Lbs.
  # Find the NA row indices and fill from aligned rows.

  indices = which(is.na(tows$Trip_Sampled_Lbs) & tows$state != "WA")
  tows$Trip_Sampled_Lbs[indices] = Trip_Sampled_medians[indices]

  # Same for WA, but the preferred value is first RWT_LBS, then TOTAL_WGT.

  indices = which(is.na(tows$Trip_Sampled_Lbs) & tows$state=="WA")
  tows$Trip_Sampled_Lbs[indices] = RWT_LBS_medians[indices]

  indices = which(is.na(tows$Trip_Sampled_Lbs) & tows$state=="WA")
  tows$Trip_Sampled_Lbs[indices] = TOTAL_WGT_medians[indices]

  # Match Trip_Sampled_Lbs to the larger dataset.

  Pdata$Trip_Sampled_Lbs = tows$Trip_Sampled_Lbs[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  if (verbose){
    cat("\nSampled pounds per trip:\n\n")
    print(summary(Pdata$Trip_Sampled_Lbs))
  }

  if (plot != FALSE) {
    numstate <- length(unique(Pdata$state))
    par(mgp = c(2.5, 0.5, 0), mfrow = c(numstate, 1), mar = rep(0, 4),
      oma = c(4, 5, 3, 0.5))
    if (is.character(plot)) png(plot)
    for (st in unique(Pdata$state)) {
      plotdata <- subset(Pdata, state == st)
      boxplot(plotdata$Trip_Sampled_Lbs ~ plotdata$fishyr,
        ylab = "", xlab = "", xaxt = "n",
        at = unique(plotdata$fishyr), xlim = range(Pdata$fishyr))
      legend("topleft", legend = st, bty = "n")
    }
    axis(1)
    mtext(side = 1, "Expansion factor 1 numerator", outer = TRUE, line = 2)
    mtext(side = 2, "sample weight per trip (lbs)", outer = TRUE, line = 2)

    if (is.character(plot)) dev.off()
  }

  return(Pdata)

} # End function EF1_Numerator

