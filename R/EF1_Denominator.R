##############################################################################
#
# EF1_Denominator calculates the denominator for the level-1 expansion factor,
# the weight of sampled fish in the tow.
#
# Indiv_Wgts controls whether or not individual Length-Weight calculations will
# be used.
#
# The weight of the fish is calculated three ways.
#
# First, FEMALES_WGT and MALES_WGT are summed per SAMPLE_NO.
#
# The SPECIES_WGT is summed across all clusters in a sample to provide
# the second per SAMPLE_NO weight.
#
# Finally, weights of the male and female fish are calculated from their
# lengths.  These are summed per SAMPLE_NO to provide a per-sample weight.
# Zero weights might occur for some fish. These are filled in with the
# median weight of fish in the sample.
#
# Arguments:
#
#    Pdata              the data set.
#    Indiv_Wgts         whether or not to calculate individual fish weights
#                       from a length-weight relationship.
#
#    fa, fb             the coefficient and factor for female Length-Weights.
#    ma, mb             the coefficient and factor for male Length-Weights.
#    ua, ub             the coefficient and factor for unsexed Length-Weights.
#
# New columns:
#
#    Wt_Sampled_1    per-SAMPLE_NO summed MALES_WGT + FEMALES_WGT
#    Wt_Sampled_2    per-SAMPLE_NO summed cluster_wt
#    LW_Calc_Wt        individual weights predicted from L-W
#    Wt_Sampled_3    per-SAMPLE_NO summed LW_Calc_Wts
#
#    Wt_Sampled        per-SAMPLE_NO combined weights.
#                       This is preferentially Wt_Sampled_1, with NAs
#                       replaced with values from Wt_Sampled_2, and NAs
#            remaining replaced with values from Wt_Sampled_3.
#
#    Wt_Method        Denotes by which method the Wt_Sampled was filled.
#
#
##############################################################################

EF1_Denominator = function( Pdata, Indiv_Wgts=TRUE,
                          fa=2e-06, fb=3.5, ma=2e-06, mb=3.5, ua=2e-06, ub=3.5 ) {

# For testing: fa=2e-06; fb=3.5; ma=2e-06; mb=3.5; ua=2e-06; ub=3.5


  # Clean up.  Muddled results if this function has been previously run.

  Pdata$Wt_Sampled_1   = NA
  Pdata$Wt_Sampled_2   = NA
  Pdata$LW_Calc_Wt     = NA
  Pdata$Wt_Sampled_3   = NA
  Pdata$Wt_Sampled     = NA
  Pdata$Wt_Method      = NA

  cat("\nGetting level-1 expansions\n\n")

  if ( Indiv_Wgts == TRUE ) {

    cat("\nIndividual weights will be generated from the following values:\n\n")
    cat("Females:", fa,fb, "Males:",  ma,mb, "Unknowns:",  ua,ub, "\n\n")

  } # End if

  # Assign 'state' if it's not already there.

  if (length(which(names(Pdata) == "state")) == 0) {

    cat("State variable is not assigned, getting state.\n\n")
    Pdata = getState(Pdata, CLEAN=T)

  }

  # Everything is calculated in terms of unique samples.

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  cat("\nGetting the summed weights of the sexed fish for each SAMPLE_NO\n\n")

  tows$Wt_Sampled_1 = tows$MALES_WGT + tows$FEMALES_WGT

  Pdata$Wt_Sampled_1 = tows$Wt_Sampled_1[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  cat("\nSumming the SPECIES_WEIGHTS for all clusters in a SAMPLE_NO\n\n")

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

    cat("\nCalculating individual weights from lengths.\n\n")

    ############################################################################
    #
    # Create a predicted fish weight based on sex and length
    # (use mm for fitted regression coefficients!)
    # these will be summed to give the sample weight
    #
    ############################################################################

    Pdata$LW_Calc_Wt = NA
    Pdata$LW_Calc_Wt[Pdata$SEX=="F"] = fa*Pdata$length[Pdata$SEX=="F"]^fb
    Pdata$LW_Calc_Wt[Pdata$SEX=="M"] = ma*Pdata$length[Pdata$SEX=="M"]^mb
    Pdata$LW_Calc_Wt[Pdata$SEX=="U"] = ua*Pdata$length[Pdata$SEX=="U"]^ub

    # Convert to pounds

    Pdata$LW_Calc_Wt = Pdata$LW_Calc_Wt * 0.00220462

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

  cat("\nDone calculating sample weights\n\n")

  # Summary and boxplot

  printemp = data.frame(cbind(Pdata$Wt_Sampled_1, Pdata$Wt_Sampled_2,
                              Pdata$Wt_Sampled_3, Pdata$Wt_Sampled))

  names(printemp) = c("M+F","SPECIES_WT","L-W","Final Wt_Sampled")

  print(summary(printemp))

  cat("\nWt_Methods:\n\n")

  print(summary(as.factor(Pdata$Wt_Method)))


  par(mfrow=c(2,2))


  boxplot(as.data.frame(printemp), names=c("M+F","SPECIES","L-W","Final Wt"),
          main = "Weight Sampled (Lbs) -- Denominator")

  NA_Wt_Sampled = Pdata[is.na(Pdata$Wt_Sampled),]
  nNA = nrow(NA_Wt_Sampled)

  if ( nNA > 0 ) {

    barplot(xtabs(NA_Wt_Sampled$FREQ ~ NA_Wt_Sampled$state + NA_Wt_Sampled$fishyr),
            col=rainbow(3),
            legend.text = T, xlab = "Year", ylab = "Samples",
            main="NA Sampled Wts -- Denominator")

  } else {

    cat("\nSample Wts found for all samples.\n\n")

  } # End if

  return(Pdata)

} # End EF1_Denominator

