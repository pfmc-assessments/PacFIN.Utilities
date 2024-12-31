#' First-Stage expansion for composition data
#'
#' First-stage expansions account for unsampled fish in the smallest measured
#' unit. Where, in PacFIN data, the smallest measured unit typically a trip
#' because that is what is available to the port or dockside sampler. Whereas,
#' in survey data the smallest measured unit is typically a tow. Tow would be
#' the smallest if we had samples from onboard observers rather than from
#' dockside samplers.
#'
#' @export
#' @seealso
#' * [cleanPacFIN()] (upstream)
#' * [nwfscSurvey::estimate_weight_length()] (upstream())
#' * [EF1_Numerator()] (contained within)
#' * [EF1_Denominator()] (contained within)
#' * [getExpansion_2()] (downstream)
#'
#' @details
#' The workflow is to run this function [cleanPacFIN(),
#' which assures that all of the necessary columns are available and that the
#' data are in the correct units. This function then calls two helper functions,
#' [EF1_Numerator()] and [EF1_Denominator()] to calculate the weight of sampled fish and the weight of all fish of the respective species in the tow, respectively. Finally, the ratio of the two values is returned.
#'
#' @section Expansion:
#' \itemize{
#' \item{Age data are expanded separately from lengths.}
#' \item{WA fish are generally only expanded using Expansion_Factor_2.}
#' \item{Other expansions are the product of
#'   Expansion_Factor_1 * Expansion_Factor_2.
#' }
#' \item{For age-at-length comps, set Final_Expansion_Factor to 1 because
#'   each fish represents only itself.}
#' }
#'
#' @inheritParams cleanPacFIN
#' @param maxExp The maximum expansion factor (either a number or a quantile)
#'   for building expansions. Typically, the default is 0.95. Set `maxExp =
#'   Inf` to see largest values.
#' @param Exp_WA A logical values specifying if the samples from Washington
#'   should be exanded. The default is `FALSE`.
#' @param fa,ma,ua Female-, male-, and unsexed-specific weight--length
#'   coefficients for Stock Synthesis where the relationships were calculated
#'   using length in cm and weight in kg. There are no default values. You must
#'   calculate these values and pass them to the function. If a particular sex
#'   does not pertain to your data, then just pass `NA` for that relationship.
#' @param fb,mb,ub Female-, male, and unsexed-specific weight--length exponents
#'   for Stock Synthesis where the relationships were calculated using length in
#'   cm and weight in kg. If a particular sex does not pertain to your data,
#'   then just pass `NA` for that relationship.
#' @param plot Typically, a logical is passed defining if you would like the
#'   default plots to be created but users can also pass a string providing a
#'   path to a directory where those plots will be stored. The default is
#'   `FALSE` and no figures will be created unless this is changed. If `TRUE` is
#'   passed, then the figures will be saved to your current working directory.
#' @template savedir
#'
#' @examples
#' \dontrun{
#' # Calculate the weight--length parameters for input to this function
#' bds_survey <- nwfscSurvey::pull_bio(
#'   common_name = "widow rockfish",
#'   survey = "NWFSC.Combo"
#'   )
#' pars <- nwfscSurvey::estimate_weight_length(
#'   data = bds_survey,
#'   col_length = "length_cm",
#'   col_weight = "weight_kg",
#'   verbose = FALSE
#'  )
#' data_exp1 <- getExpansion_1(
#'   Pdata = bds_cleaned,
#'   fa = weight_length_estimates |>
#'   dplyr::filter(sex == "female") |>
#'     dplyr::pull("A"),
#'   fb = weight_length_estimates |>
#'     dplyr::filter(sex == "female") |>
#'     dplyr::pull("B"),
#'   ma = weight_length_estimates |>
#'     dplyr::filter(sex == "male") |>
#'     dplyr::pull("A"),
#'   mb = weight_length_estimates |>
#'     dplyr::filter(sex == "female") |>
#'     dplyr::pull("B"),
#'   ua = weight_length_estimates |>
#'     dplyr::filter(sex == "all") |>
#'     dplyr::pull("A"),
#'   ub = weight_length_estimates |>
#'     dplyr::filter(sex == "all") |>
#'     dplyr::pull("B"),
#'   maxExp = 0.95)
#' 
#' }
#' @return
#' A `data.frame` where all of the original columns in `Pdata` remain unaltered
#' but additional columns are added. In particular columns starting with
#' `Expansion_Factor_1` are available for setting the `Final_Expansion_Factor`.
#'
getExpansion_1 <- function(Pdata,
                           fa,
                           fb,
                           ma,
                           mb,
                           ua,
                           ub,
                           maxExp = 0.95,
                           Exp_WA = TRUE,
                           verbose = TRUE,
                           plot = lifecycle::deprecated(),
                           savedir = NULL) {
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "getExpansion_1(plot)",
      details = "Please use savedir to create and save plots."
    )
  }

  nwfscSurvey::check_dir(dir = savedir, verbose = verbose)

  Pdata <- EF1_Denominator(
    Pdata,
    fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub,
    verbose = verbose,
    savedir = savedir
  )

  # Get Trip_Sampled_Lbs
  Pdata <- EF1_Numerator(Pdata, verbose = verbose, savedir = savedir)

  # Expansion_Factor_1
  Pdata$Expansion_Factor_1_L <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_L
  Pdata$Expansion_Factor_1_A <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_A

  Pdata$Expansion_Factor_1_L[Pdata$Expansion_Factor_1_L < 1] <- 1
  Pdata$Expansion_Factor_1_A[Pdata$Expansion_Factor_1_A < 1] <- 1

  # In most cases, WA data can't be expanded.
  if (Exp_WA != TRUE) {
    Pdata$Expansion_Factor_1_L[Pdata$state == "WA"] <- 1
    Pdata$Expansion_Factor_1_A[Pdata$state == "WA"] <- 1
    cli::cli_bullets(c(
      "i" = "Fish tickets do not represent whole trips in WA.",
      "i" = "WA expansions set to 1 because {.code Exp_WA = {Exp_WA}}."
    ))
  }

  NA_EF1 <- Pdata[is.na(Pdata$Expansion_Factor_1_L), ]
  nNA <- NROW(NA_EF1)

  if (verbose) {
    cli::cli_alert_info(
      "{NROW(NA_EF1)} {.code NA} Expansion_Factor_1 values replaced by 1."
    )
  }

  # Now replace NAs with 1.
  Pdata$Expansion_Factor_1_L[is.na(Pdata$Expansion_Factor_1_L)] <- 1
  Pdata$Expansion_Factor_1_A[is.na(Pdata$Expansion_Factor_1_A)] <- 1
  # Now replace Inf with 1
  Pdata$Expansion_Factor_1_L[!is.finite(Pdata$Expansion_Factor_1_L)] <- 1
  Pdata$Expansion_Factor_1_A[!is.finite(Pdata$Expansion_Factor_1_A)] <- 1
  
  Pdata$Expansion_Factor_1_L <- capValues(Pdata$Expansion_Factor_1_L, maxExp)
  Pdata$Expansion_Factor_1_A <- capValues(Pdata$Expansion_Factor_1_A, maxExp)
  if (verbose) {
    cli::cli_inform(
      "Maximum first-stage length expansion capped at the {maxExp} quantile of {round(max(Pdata$Expansion_Factor_1_L), 2)}"
    )
    cli::cli_inform(
      "Maximum first-stage age expansion capped at the {maxExp} quantile of {round(max(Pdata$Expansion_Factor_1_A), 2)}"
    )
  }

  # Generate plots and save them to the disk if specified.
  # TODO: move away from {grDevices}
  if (!is.null(savedir)) {
    grDevices::png(fs::path(savedir, "PacFIN_exp1.png"))

    if (nNA > 0) {
      # Plot NA values by year and state.  Early years data or CALCOM data?
      graphics::par(
        mfrow = c(2, 1), mar = c(0, 3, 0, 0), oma = c(4, 1, 3, 0),
        mgp = c(2.0, 0.5, 0)
      )

      allyears <- seq(min(Pdata$fishyr), max(Pdata$fishyr), by = 1)
      vals <- matrix(0,
        nrow = length(unique(NA_EF1$state)),
        ncol = length(allyears)
      )
      rownames(vals) <- unique(NA_EF1$state)
      colnames(vals) <- allyears
      bad <- as.matrix(table(NA_EF1$state, NA_EF1$fishyr))
      vals[, colnames(vals) %in% colnames(bad)] <- bad

      graphics::barplot(vals,
        col = grDevices::rainbow(length(unique(NA_EF1$state))),
        legend.text = TRUE, xlab = "", xaxt = "n",
        ylab = "Replace NA in Exp_1 with 1",
        args.legend = list(bty = "n")
      )
    } else {
      graphics::par(
        mgp = c(2, 0.5, 0),
        mar = c(1.5, 3, 1, 0),
        mfrow = c(1, 1)
      )
    } # End if

    graphics::boxplot(Pdata$Expansion_Factor_1_L ~ Pdata$fishyr,
      ylab = "Expansion_Factor_1_L",
      xlab = "", frame.plot = FALSE
    )

    graphics::mtext(side = 1, outer = TRUE, "Year", line = 2)
    grDevices::dev.off()
  }

  return(Pdata)
} # End function getExpansion_1
