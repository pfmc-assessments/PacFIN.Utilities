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
#' \item Wt_Sampled_1
#' \item Wt_Sampled_2
#' \item LW_Calc_Wt: individual weights predicted from the specified length-weight relationships.
#' \item Wt_Sampled_3
#' \item Wt_Sampled: the sample weight that will be used in subsequent analyses,
#'   where this is preferentially Wt_Sampled_1, with NAs replaced with values from Wt_Sampled_2,
#'   and NAs remaining replaced with values from Wt_Sampled_3.
#' \item Wt_Method: a \code{numeric} value denoting which method was used for \code{Wt_Sampled}.
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
    cat("Females:", fa,fb, "Males:",  ma,mb, "Unknowns:",  ua,ub, "\n\n")

  } # End if

  # Assign 'state' if it's not already there.

  if (length(which(names(Pdata) == "state")) == 0) {

    if (verbose){
      cat("State variable was not assigned, getting state.\n\n")
    }
    Pdata = getState(Pdata, CLEAN=T, verbose = verbose)

  }

  # Everything is calculated in terms of unique samples.

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  tows$Wt_Sampled_1 = tows$MALES_WGT + tows$FEMALES_WGT

  Pdata$Wt_Sampled_1 = tows$Wt_Sampled_1[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  Pdata$SAMP_CLUST = paste(Pdata$SAMPLE_NO, Pdata$CLUSTER_NO, sep="_")
  uniqueClusters = Pdata[!duplicated(Pdata$SAMP_CLUST),]

  tmp = aggregate(uniqueClusters$SPECIES_WGT, list(uniqueClusters$SAMPLE_NO), sum, na.rm=T)
  names(tmp) = c("SAMPLE_NO", "wgt")

  # Might have 0 values that should actually be NA.  Aggregate doesn't generate NAs.

  tmp$wgt[tmp$wgt == 0] = NA

  tows$Wt_Sampled_2 = tmp$wgt[match(tows$SAMPLE_NO,tmp$SAMPLE_NO)]

  Pdata$Wt_Sampled_2 = tmp$wgt[match(Pdata$SAMPLE_NO,tmp$SAMPLE_NO)]

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
    Pdata$LW_Calc_Wt[Pdata$SEX=="U"] = ua*(Pdata$length[Pdata$SEX=="U"] / 10)^ub

    # Convert to pounds

    Pdata$LW_Calc_Wt = Pdata$LW_Calc_Wt * 2.20462
    #KFJ(2015-06-20): I think this calculation is wrong and SS
    # uses parameters that assume length is in cm and weight
    # is in kg, thus making the correct calculation
    # a * (Pdata$length / 10)^b * 2.20462

    # Get the number of observed lengths and weights to use for each sample

    tmp = as.data.frame(table(Pdata$SAMPLE_NO))
    names(tmp) = c("SAMPLE_NO","numlens")

    tows$numlens = tmp$numlens[match(tows$SAMPLE_NO, tmp$SAMPLE_NO)]

    # Note:  if all else fails, fill 0 weights with the median in that sample.

    tmp_wt = aggregate(Pdata$LW_Calc_Wt, list(Pdata$SAMPLE_NO), sum, na.rm=T)
    med_wt = aggregate(Pdata$LW_Calc_Wt, list(Pdata$SAMPLE_NO), median, na.rm=T)
    tmp_wt = cbind(tmp_wt, med_wt[,2])
    names(tmp_wt) = c("SAMPLE_NO","Wt_Sampled_3","Median")

    tmp_wt$Wt_Sampled_3[tmp_wt$Wt_Sampled_3 == 0] = tmp_wt$Median[tmp_wt$Wt_Sampled_3 == 0]

    tows$Wt_Sampled_3 = tmp_wt$Wt_Sampled_3[match(tows$SAMPLE_NO, tmp_wt$SAMPLE_NO)]

    Pdata$Wt_Sampled_3 = tows$Wt_Sampled_3[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]
    Pdata$numlens = tows$numlens[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  } else {

    # Need for summary and boxplot

    Pdata$Wt_Sampled_3 = NA

  } # End if-else Indiv_Wgts


  ############################################################################
  #
  # Use calculated weights for Wt_Sampled.
  #
  ############################################################################

  tows$Wt_Sampled = tows$Wt_Sampled_1
  tows$Wt_Method = 1

  tows$Wt_Method[is.na(tows$Wt_Sampled)] = 2
  tows$Wt_Sampled[is.na(tows$Wt_Sampled)] = tows$Wt_Sampled_2[is.na(tows$Wt_Sampled)]

  if (Indiv_Wgts) {

    tows$Wt_Method[is.na(tows$Wt_Sampled)] = 3
    tows$Wt_Sampled[is.na(tows$Wt_Sampled)] = tows$Wt_Sampled_3[is.na(tows$Wt_Sampled)]

  } # End if

  tows$Wt_Method[is.na(tows$Wt_Sampled)] = NA

  # Match per-tow data to whole dataset.

  Pdata$Wt_Method = tows$Wt_Method[match(Pdata$SAMPLE_NO,tows$SAMPLE_NO)]
  Pdata$Wt_Sampled = tows$Wt_Sampled[match(Pdata$SAMPLE_NO,tows$SAMPLE_NO)]



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

