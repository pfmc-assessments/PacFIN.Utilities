###########################################################################
#
#' Calculate the denominator for the level-1 expansion factor.
#' 
#' \subsection{\code{\link{Workflow}}}{
#' \code{EF1_Denominator} is not run by the user.  It is a sub-function of 
#' \code{\link{getExpansion_1}}
#' }
#' 
#'
#' The denominator of the level-1 expansion factor is the weight of the sampled
#' fish in a tow. The calcuation is done one in three ways, all of which
#' are returned, though the column used in subsequent analyses is based on
#' the hierarchical structure of:
#' \enumerate{
#' \item{\code{sum(Pdata$FEMALES_WGT + Pdata$MALES_WGT)} per unique \code{SAMPLE_NO}}
#' \item{\code{sum(Pdata$SPECIES_WGT)} across all clusters in a sample, where the
#'   species weight is determined from the \code{cluster_wt}}
#' \item{Calculate weights of males and females given the weight length relationship,
#' if the length of a fish does not exist, then the median weight of all
#' weighed fish in the sample is used.}
#' }
#'
#' @template Pdata
#' @template Indiv_Wgts
#' @template weightlengthparams
#' @param verbose Report extra information.  Default:  TRUE
#' @param plot Create plots.  Default:  FALSE
#' @return Additional columns are added to \code{Pdata}:
#' \itemize{
#' \item Wt_Sampled_1: the sum of weights for male and female fish within the
#' sample, this will ignore unsexed fish or hermaphrodites. 
#' \item Wt_Sampled_2: the species-specific sample weight only provided by
#'   California because the cluster weight could include multiple species.
#' \item LW_Calc_Wt: individual weights predicted from the specified length-weight relationships.
#' \item Wt_Sampled_3: The sum of empirical weights, for those fish within a 
#' sample where this information is available, and weights calculated from the
#' length-weight relationship. This uses the empirical data if available and 
#' fills in with the expected weight.
#' \item Wt_Sampled: the sample weight that will be used in subsequent analyses,
#'   where this is preferentially the empirical weights; all NA values are 
#'   subsequently filled in using Wt_Sampled_1, 
#'   with NAs replaced with values from Wt_Sampled_2,
#'   and NAs remaining replaced with values from Wt_Sampled_3.
#' \item Wt_Method: a \code{numeric} value starting with zero for empirical weights
#'   and then denoting which method was used for \code{Wt_Sampled}.
#' }
#' @author Andi Stephens
#' @seealso \code{\link{EF1_Numerator}}, \code{\link{getExpansion_1}}, \code{\link{getExpansion_2}}
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#
#
###########################################################################

EF1_Denominator = function( Pdata, Indiv_Wgts=TRUE,
                            fa = 2e-06, fb = 3.5, 
                            ma = 2e-06, mb = 3.5, 
                            ua = 2e-06, ub = 3.5, 
                            verbose = TRUE, 
                            plot = FALSE) 
{

# For testing: fa=2e-06; fb=3.5; ma=2e-06; mb=3.5; ua=2e-06; ub=3.5


# Clean up.  Muddled results if this function has been previously run.

  Pdata$Wt_Sampled_1   = NA
  Pdata$Wt_Sampled_2   = NA
  Pdata$LW_Calc_Wt     = NA
  Pdata$Wt_Sampled_3   = NA
  Pdata$Wt_Sampled     = NA
  Pdata$Wt_Method      = NA

  if ( Indiv_Wgts == TRUE & verbose) {

    cat("\nIndividual weights will be generated from the following values:\n\n")
    cat(" Females:", fa,fb, "\n",
        "Males:",  ma,mb, "\n",
        "Unknowns and hermaphrodites:",  ua,ub, "\n\n")

  } # End if

  # Assign 'state' if it's not already there.

  if (length(which(names(Pdata) == "state")) == 0) {

    if (verbose){
      cat("State variable was not assigned, getting state.\n\n")
    }
    Pdata = getState(Pdata, CLEAN=T, verbose = verbose)

  }

  # Everything is calculated in terms of unique samples.
  # Calculate the sampled weight based on weights of individual fish
  Pdata$Wt_Sampled <- ave(Pdata$FISH_WEIGHT,
    Pdata$SAMPLE_NO, FUN = sum)
  Pdata$unsexed_num <- ave(Pdata$SEX, Pdata$SAMPLE_NO,
    FUN = function(x) sum(x %in% c("U", "H")))
  # KFJ(2019-03-21): Could get this column from PacFIN directly.
  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  #### Oregon - MALES_WGT and FEMALES_WGT is only available from Oregon.
  # Allow sum to be calculated when there are no males or no females
  # because weights are NA in those instances rather than a value of zero.
  tows$Wt_Sampled_1 <- ifelse(is.na(tows$MALES_WGT) & is.na(tows$FEMALES_WGT),
    NA,
    apply(tows[, c("MALES_WGT", "FEMALES_WGT")], 1, sum, na.rm = TRUE))

  Pdata$Wt_Sampled_1 = tows$Wt_Sampled_1[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  #### California - multiple species can be sampled in one sample number
  # SPECIES_WGT is specific to a cluster, so sum the species weight across clusters
  # within a given sample
  Pdata$Wt_Sampled_2 <- unsplit(
    # For every sample_no get the species_wgt for a given cluster
    lapply(split(Pdata, list(Pdata$SAMPLE_NO)), 
      function(x) sum(x[!duplicated(x$CLUSTER_NO), "SPECIES_WGT"])), 
    # Assign each answer to entries with that sample_no in Pdata
    Pdata$SAMPLE_NO)

  #### Washington b/c there is no other method to find the sample weight.
  # Only if there are individual weight factor and coefficients available

  if (Indiv_Wgts) {

    ############################################################################
    #
    # Create a predicted fish weight based on sex and length
    # (use mm for fitted regression coefficients!)
    # these will be summed to give the sample weight
    #
    ############################################################################

    Pdata$LW_Calc_Wt = NA
    Pdata$LW_Calc_Wt[Pdata$SEX=="F"] = fa*(Pdata$length[Pdata$SEX=="F"] / 10)^fb
    Pdata$LW_Calc_Wt[Pdata$SEX=="M"] = ma*(Pdata$length[Pdata$SEX=="M"] / 10)^mb
    Pdata$LW_Calc_Wt[Pdata$SEX %in% c("U", "H")] = ua*(Pdata$length[Pdata$SEX %in% c("U", "H")] / 10)^ub

    # Convert to pounds

    Pdata$LW_Calc_Wt = Pdata$LW_Calc_Wt * 2.20462
    #KFJ(2015-06-20): I think this calculation is wrong and SS
    # uses parameters that assume length is in cm and weight
    # is in kg, thus making the correct calculation
    # a * (Pdata$length / 10)^b * 2.20462
    
    bestweight <- ifelse(is.na(Pdata$FISH_WEIGHT),
      Pdata$LW_Calc_Wt, Pdata$FISH_WEIGHT)
    Pdata$Wt_Sampled_3 <- ave(bestweight, Pdata$SAMPLE_NO,
      FUN = function(x) sum(x, na.rm = TRUE))

  } else {

    # Need for summary and boxplot

    Pdata$Wt_Sampled_3 = NA

  } # End if-else Indiv_Wgts


  ############################################################################
  #
  # Use calculated weights for Wt_Sampled.
  #
  ############################################################################

  Pdata$Wt_Method <- 0
  # Method 1 ignores unsexed fish b/c it is just the sum of males and females
  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled) & Pdata$unsexed_num == 0] = 1
  Pdata$Wt_Sampled[Pdata$Wt_Method == 1] = Pdata$Wt_Sampled_1[Pdata$Wt_Method == 1]

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = 2
  Pdata$Wt_Sampled[is.na(Pdata$Wt_Sampled)] = Pdata$Wt_Sampled_2[is.na(Pdata$Wt_Sampled)]

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = 3
  Pdata$Wt_Sampled[is.na(Pdata$Wt_Sampled)] = Pdata$Wt_Sampled_3[is.na(Pdata$Wt_Sampled)]

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = NA

  # Summary and boxplot

  printemp = data.frame(cbind(Pdata$Wt_Sampled_1, Pdata$Wt_Sampled_2,
                              Pdata$Wt_Sampled_3, Pdata$Wt_Sampled))

  names(printemp) = c("M+F","SPECIES_WT","L-W","Final Wt_Sampled")

  if (verbose) {
    cat("\nDone calculating sample weights\n\n")
    print(summary(printemp))
    cat("\nWt_Methods:\n\n")
    print(summary(as.factor(Pdata$Wt_Method)))
  }

  NA_Wt_Sampled <- Pdata[is.na(Pdata$Wt_Sampled), ]
  nNA <- NROW(NA_Wt_Sampled)

  if (plot != FALSE){
    if (is.character(plot)) png(plot)
    par(mfrow = c(1, ifelse(nNA > 0, 2, 1)), mgp = c(2.5, 0.5, 0))
    boxplot(as.data.frame(printemp),
      names = c("M+F", "species", "pred. w/ L-W","final"),
      ylab = "Sample weight (lbs)", xlab = "Expansion factor 1 denominator")
    if (nNA > 0) {
      barplot(xtabs(NA_Wt_Sampled$FREQ ~ NA_Wt_Sampled$state + NA_Wt_Sampled$fishyr),
        col = rainbow(length(unique(NA_Wt_Sampled$state))),
        legend.text = TRUE, xlab = "Year",
        ylab = "Num samples w/ denominator = NA",
        args.legend = list(x = "topleft", bty = "n"))
    }
    if (is.character(plot)) dev.off()
  }
  if (nNA == 0 & verbose) cat("\nSample Wts found for all samples.\n\n")

  return(Pdata)

} # End EF1_Denominator

