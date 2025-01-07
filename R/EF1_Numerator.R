#' Calculate the numerator for the first level expansion factor
#'
#' Calculate the numerator for the first-level expansion factor, where
#' the numerator is the species-specific landing weight for a given sample.
#' Thus, if two clusters were sampled from a single trip,
#' they would both use the same landing weight.
#'
#' @details
#' Previously, `Trip_Sampled_Lbs` was calculated differently for each state.
#' For California, `Species_Percent_Sampled * TOTAL_WGT`.
#' For Oregon, `Pdata$EXP_WT` and if missing, the same as California.
#' For Washington, `Pdata$RWT_LBS`, `Pdata$TOTAL_WGT`, `RWT_LBS`, or
#' `median(Pdata$TOTAL_WGT)`.
#' Then, if all else failed, per-year, state-specific medians.
#'
#' Now, PacFIN works hard behind the scenes to provide species-specific landing
#' weights for each sampled fish. Therefore, we no longer rely on code to
#' calculate a fabricated landing weight. Species-specific landing weights are
#' available in either `EXPANDED_SAMPLE_WEIGHT` or `WEIGHT_OF_LANDING_LBS`.
#' The former, is specific to Oregon and samples that do not provide an expanded
#' sample weight should not be used more than likely anyway.
#'
#' **todo**:
#' * determine if we want to flag some bad samples in [cleanPacFIN].
#' * fix up the plotting and summary code
#'
#' @seealso [getExpansion_1] calls this function.
#' @return A \code{Pdata} with additional columns, where
#' `Trip_Sampled_Lbs` is the sample weight in pounds.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#' @template plot
#' @template savedir
#' @author Andi Stephens, Kelli F. Johnson, Chantel R. Wetzel

EF1_Numerator <- function(Pdata,
                          verbose = TRUE,
                          plot = lifecycle::deprecated(),
                          savedir = NULL) {
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "EF1_Denominator(plot)",
      details = "Please use savedir to create and save plots."
    )
  }
  Pdata$Trip_Sampled_Lbs <- dplyr::coalesce(
    Pdata[["EXP_WT"]], Pdata[["RWT_LBS"]]
  )

  if (verbose) {
    cli::cli_inform("Sampled pounds per trip:")
    print(summary(Pdata$Trip_Sampled_Lbs))
  }

  if (!is.null(savedir)) {
    numstate <- length(unique(Pdata$state))
    plot_filename <- fs::path(
      savedir,
      "PacFIN_exp1_numer.png"
    )
    grDevices::png(plot_filename)
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    graphics::par(
      mgp = c(2.5, 0.5, 0), mfrow = c(numstate, 1), mar = rep(0, 4),
      oma = c(4, 5, 3, 0.5)
    )
    for (st in unique(Pdata$state)) {
      plotdata <- Pdata[Pdata[, "state"] == st & !is.na(Pdata[["Trip_Sampled_Lbs"]]), ]
      if (all(is.na(plotdata$Trip_Sampled_Lbs))) next
      graphics::boxplot(plotdata$Trip_Sampled_Lbs ~ plotdata$fishyr,
        ylab = "", xlab = "", xaxt = "n",
        at = unique(plotdata$fishyr), xlim = range(Pdata$fishyr)
      )
      graphics::legend("topleft", legend = st, bty = "n")
    }
    graphics::axis(1)
    graphics::mtext(side = 1, "Year", outer = TRUE, line = 2)
    graphics::mtext(side = 3, "Expansion factor 1 numerator", outer = TRUE, line = 1)
    graphics::mtext(side = 2, "Sample weight per trip (lbs)", outer = TRUE, line = 2)
  }

  return(Pdata)
} # End function EF1_Numerator
