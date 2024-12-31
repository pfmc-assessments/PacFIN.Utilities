#' Calculate the denominator for the level-1 expansion factor.
#'
#' Calculate the denominator for the level-1 expansion, where the denominator
#' is the weight of all fish in the sample.
#' `EF1_Denominator` is not run by the user, it is a sub-function of
#' [getExpansion_1].
#'
#' @details
#' The denominator of the level-1 expansion factor is the weight of the sampled
#' fish in the sample unit, where for fisheries data, this unit is most often
#' the trip level because information on individual hauls is not available.
#' For a survey, tow- or haul-level data are typically available.
#' The sum of the weight in the sample is calculated different for each state
#' based on the data that are available.
#'
#' Oregon provides information on the weight of females and males in the sample
#' via the `WEIGHT_OF_FEMALES` and `WEIGHT_OF_MALES` columns. These are often
#' model-based weights, which would be the only way they can get weights when
#' the fish were not weighed themselves. The weight of unsexed fish is calculated
#' internally by the code and added to the female and male weight.
#' **todo**: Let Oregon know that this calculation is being done and they may want
#' to provide UNK_WGT.
#'
#' California sample weights were previously based on the column labeled
#' `SPECIES_WEIGHT`. Now, California data is parsed by PacFIN to furnish
#' species-specific cluster weights. Prior, cluster weights included the weight
#' of all species in the sample. Now, `CLUSTER_WEIGHT` is the weight in that
#' cluster for the species of interest. This was verified for dover sole by
#' Kelli F. Johnson in February of 2021. Thus, the code uses `CLUSTER_WEIGHT`
#' rather than `SPECIES_WEIGHT` now.
#' **todo**: determine if this should be the only cluster-specific value going forward?
#'
#' Washington does not pretend to provide any information regarding total weight
#' of fish in the sample. Therefore, this value is calculated by summing the
#' empirical weight of fish. For fish that were not weighed, a weight is
#' calculated given the input parameters. It is up to the user to calculate
#' these if they want them based on more data than what is included in `Pdata`.
#' Finally, for fish that have no length and therefore no predicted weight,
#' the mean of fish weight in the sample is used to assign all fish a weight.
#'
#' Another thing to note in the calculation of the denominator is that sample
#' weights are length- and age-specific values. You would not want to weight
#' an age sample based on how many lengths were measured. So, the weights of
#' fish that are not lengthed are backed out of the sample weight and the
#' weight of fish that are not aged are backed out of the sample weight to
#' create length- and age-specific sample weights. Only these specific sample
#' weights should be used going forward.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams getExpansion_1
#' @inheritParams cleanPacFIN
#' @param plot A logical that specifies if plots should be created or not.
#' The default is `FALSE`.
#' @param col.weight The name of column that contains the weight of the fish
#' in kilograms. Units are important here because kilograms will be converted
#' to pounds to match other weight calculations.
#' The default is `weightkg`, which is created using [cleanPacFIN].
#' @template savedir
#' @return Additional columns are added to \code{Pdata}:
#' * `Wt_Sampled_1`: the sum of sex-specific weights within the sample.
#' * `Wt_Sampled_2`: the species-specific sample weight only provided by
#'   California in cluster weight.
#' * `LW_Calc_Wt`: individual weights predicted from the specified
#'   length-weight relationships.
#' * `Wt_Sampled_3`: The sum of empirical weights, for those fish within a
#'   sample where this information is available, and weights calculated from the
#'   length-weight relationship. This uses the empirical data if available and
#'   fills in with the expected weight or mean-sample weight.
#'
#' @author Andi Stephens
#' @seealso [EF1_Numerator], [getExpansion_1], [getExpansion_2]
#'
EF1_Denominator <- function(Pdata,
                            fa,
                            fb,
                            ma,
                            mb,
                            ua,
                            ub,
                            verbose = TRUE,
                            plot = lifecycle::deprecated(),
                            col.weight = "weightkg",
                            savedir = NULL) {
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "EF1_Denominator(plot)",
      details = "Please use savedir to create and save plots."
    )
  }
  
  if (verbose) {
    cli::cli_inform(c(
      "i" = "Individual weights will be generated from the passed parameters.",
      "i" = "Females: a = {.val {signif(fa, 3)}} and b = {.val {round(fb, 3)}}",
      "i" = "Males: a = {.val {signif(ma, 3)}} and b = {.val {round(mb, 3)}}",
      "i" = "Unsexed and hermaphrodites: a = {.val {signif(ua, 3)}} and b = {.val {round(ub, 3)}}"
    ))
  }

  stopifnotcolumn(Pdata, col.weight)

  # Calculate weight based on length--weight relationship
  # TODO: Check that this function can handle NA values for the weight--length
  #       parameters if the sex does not exist. What happens if missing values
  #       are in getExpansion_1?
  Pdata$LW_Calc_Wt <- getweight(
    length = Pdata$length,
    sex = Pdata$SEX,
    pars = data.frame(
      "A" = c("females" = fa, "males" = ma, "all" = ua),
      "B" = c("females" = fb, "males" = mb, "all" = ub)
    ),
    unit.out = "lb"
  )

  #### Calculate sample weight using FISH_WEIGHT in lbs
  # TODO: use !! instead of the actual column name
  # TODO: use native pipe |>
  Pdata <- Pdata %>%
    # Use weightkg if available and calculated from WL relationship when NA
    # Note the change in units for weightkg from KG to LBS
    dplyr::mutate(
      bestweight = dplyr::case_when(
        is.na(weightkg) ~ LW_Calc_Wt,
        TRUE ~ weightkg * 2.20462
      )
    ) %>%
    # Group by SAMPLE_NO so all subsequent calculations are done on subsets
    # of the data, i.e., mean(bestweight) is mean of the bestweight in a
    # specific sample
    dplyr::group_by(SAMPLE_NO) %>%
    dplyr::mutate(
      bestweight = ifelse(
        is.na(bestweight),
        mean(bestweight),
        bestweight
      )
    ) %>%
    # Calculate sample weights and weight of unsexed fish per SAMPLE_NO
    dplyr::mutate(
      Wt_Sampled_3_L = sum(
        na.rm = TRUE,
        ifelse(is.na(length), NA, bestweight)
      ),
      Wt_Sampled_3_A = sum(
        na.rm = TRUE,
        ifelse(is.na(Age), NA, bestweight)
      ),
      UNK_WT = sum(ifelse(SEX == "U", bestweight, 0)),
      UNK_NUM = sum(SEX == "U")
    ) %>%
    # Back out the weight of fish that have no length or Age for each
    # specific sample weight, if all are NA in sample, then set to 0.
    dplyr::mutate(
      Wt_Sampled_1_A = (-1 * sum(ifelse(is.na(Age), bestweight, 0)) +
        FEMALES_WGT + MALES_WGT + UNK_WT) *
        ifelse(all(is.na(Age)), 0, 1),
      Wt_Sampled_1_L = (-1 * sum(ifelse(is.na(length), bestweight, 0)) +
        FEMALES_WGT + MALES_WGT + UNK_WT) *
        ifelse(all(is.na(length)), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SAMPLE_NO, CLUSTER_NO) %>%
    # Do the same for CLUSTER_WGT
    dplyr::mutate(
      Wt_Sampled_2_A = (-1 * sum(ifelse(is.na(Age), bestweight, 0)) +
        CLUSTER_WGT) * ifelse(all(is.na(Age)), 0, 1),
      Wt_Sampled_2_L = (-1 * sum(ifelse(is.na(length), bestweight, 0)) +
        CLUSTER_WGT) * ifelse(all(is.na(length)), 0, 1)
    ) %>%
    # Bring the calculations back to the full scale of the data frame
    dplyr::ungroup() %>%
    # Coalesce sets things to downstream values, only if NA, i.e.,
    # Wt_Sampled_[AL] is set by priority left to right 1, 2, 3
    dplyr::mutate(
      Wt_Sampled_A = dplyr::coalesce(Wt_Sampled_1_A, Wt_Sampled_2_A, Wt_Sampled_3_A),
      Wt_Sampled_L = dplyr::coalesce(Wt_Sampled_1_L, Wt_Sampled_2_L, Wt_Sampled_3_L)
    ) %>%
    # Return a data frame rather than a tibble
    data.frame()

  #### Summary and boxplot
  # TODO: revamp the summary and plots
  printemp <- data.frame(cbind(
    Pdata$Wt_Sampled_1_L, Pdata$Wt_Sampled_2_L,
    Pdata$Wt_Sampled_3_L, Pdata$Wt_Sampled_L
  ))

  names(printemp) <- c("M+F+U", "Cluster", "L-W", "Final Wt_Sampled")

  #if (verbose) {
  #  cli::cli_alert_info("Sample weights")
  #  print(summary(printemp))
  #}

  NA_Wt_Sampled <- Pdata[is.na(Pdata$Wt_Sampled_L), ]
  nNA <- NROW(NA_Wt_Sampled)

  if (!is.null(savedir)) {
    plot1 <- file.path(savedir, "PacFIN_exp1_denom.png")
    plot2 <- file.path(savedir, "PacFIN_WL.png")
    grDevices::png(plot1)
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    graphics::par(mfrow = c(1, ifelse(nNA > 0, 2, 1)), mgp = c(2.5, 0.5, 0))
    graphics::boxplot(
      as.data.frame(printemp),
      names = names(printemp),
      ylab = "Sample weight (lbs)",
      xlab = "First-stage expansion denominator"
    )
    if (nNA > 0) {
      graphics::barplot(
        stats::xtabs(
          NA_Wt_Sampled$FREQ ~ NA_Wt_Sampled$state + NA_Wt_Sampled$fishyr
        ),
        col = grDevices::rainbow(length(unique(NA_Wt_Sampled$state))),
        legend.text = TRUE, xlab = "Year",
        ylab = "N samples w/ first-stage expansion denominator = NA",
        args.legend = list(x = "topleft", bty = "n")
      )
    }
    gg <- plotWL(
      Pdata[, "lengthcm"], Pdata[, "SEX"], Pdata[, "weightkg"],
      Pdata[, "LW_Calc_Wt"] * 0.453592
    )
    ggplot2::ggsave(
      gg,
      file = plot2,
      width = 6,
      height = 6,
      units = "in"
    )
  }
  if (nNA == 0 && verbose) {
    cli::cli_alert_success("Sample Wts found for all samples.")
  }

  return(Pdata)
}
