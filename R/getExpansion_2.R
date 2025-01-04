#' Second-stage expansion of composition samples up to catch level
#'
#' The second-stage expansion calculates the expansion factor based on the
#' ratio of total catch within a stratification (e.g., year, gear group) to the
#' amount of that catch that was sampled.
#'
#' @export
#'
#' @author Andi Stephens
#'
#' @template Pdata
#' @param Catch A data frame of catch data, in pounds or in metric tonnes.
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
#' @param stratification.cols A vector of column names in `Pdata` that you want
#' to use as strata. These will match the way in which the catches are transformed
#' from long to wide prior to inputting them into this function. If you leave
#' this argument empty, then `Pdata` must already have a column named
#' `stratification`. The function will look in the column names of the `Catch`
#' data to determine the appropriate separator to use between columns when
#' pasting the words together, which is done using [apply] and [paste].
#' Historically, it was mandatory to make this column yourself, but in 2021,
#' this input argument was added to reduce the number of extraneous calls that
#' were needed between functions.
#' You can use as many levels of stratification as you want except year because
#' it is already included in the call to [stats::aggregate].
#' @template verbose
#' @template savedir
#'
#' @seealso `getExpansion_2` is ran after [getExpansion_1] using the
#' returned data frame.
#'
#' @template secExpansion
#'
#' @return
#' The input PacFIN dataset, with column \code{Expansion_Factor_2} appended.
#'
#' @details
#' Find the catch for each year and grouping in `Catch` and divide by the
#' pounds of fish that were collected for sampling for that same year and
#' grouping. Sampled biomass is stored in `All_Trips_Sampled_Lbs`, which is
#' the sum of Trip_Sampled_Lbs across sample numbers.
#' Catches were already stratified (i.e., summed by group placed in a column
#' for a given year or row). Catches are converted to pounds prior to dividing.
#' Thus, per-stratum Expansion_Factor_2 is the catch / sampled catch.
#'
getExpansion_2 <- function(Pdata,
                           Catch,
                           Units = c("MT", "LB"),
                           Convert = NULL,
                           maxExp = 0.95,
                           stratification.cols,
                           verbose = TRUE,
                           savedir) {
  #### Set up
  # Check Unit input
  Units <- match.arg(Units,
                     several.ok = FALSE,
                     choices = c(measurements::conv_unit_options[["mass"]], "MT", "LB")
  )
  Units <- switch(Units,
                  MT = "metric_ton",
                  LB = "lbs",
                  Units
  )
  
  # Check and stop if Convert input is used since it is not deprecated
  if (!is.null(Convert)) {
    stop(
      "Convert is deprecated.",
      "Please specify the units of Catch via the Unit input (MT or LB).\n",
      paste(measurements::conv_unit_options[["mass"]], collapse = ", ")
    )
  }
  
  # Start clean
  Pdata$Expansion_Factor_2 <- NA
  if (length(Pdata$Trip_Sampled_Lbs) == 0) {
    stop("Please run getExpansion_1 first")
  } # End if
  
  # Pdata must have a "stratification" column
  if (length(Pdata$stratification) == 0) {
    if (!missing(stratification.cols)) {
      if (all(stratification.cols %in% colnames(Pdata))) {
        if (length(stratification.cols) == 1) {
          Pdata[, "stratification"] <- Pdata[, stratification.cols]
        } else {
          separate <- unique(gsub(
            "^[a-zA-Z]+(\\s*[[:punct:]]\\s*)[a-zA-Z]+$",
            "\\1", colnames(Catch)[-1]
          ))
          Pdata[, "stratification"] <- apply(Pdata[, stratification.cols],
                                             1, paste,
                                             collapse = separate
          )
        }
      } else {
        stop("Pdata must have stratification column or provide stratification.cols")
      }
    }
  } # End if
  
  # Check Catch columns against Pdata
  # Ensure Year is first column
  yearcol <- grep("year", colnames(Catch), ignore.case = TRUE)
  Catch <- Catch[, c(yearcol, seq(1:NCOL(Catch))[-yearcol])]
  Catchgears <- sort(names(Catch)[-1])
  Pstrat <- sort(unique(Pdata$stratification))
  
  if (!identical(Pstrat, Catchgears)) {
    message("Error:  mismatch between dataset and catch.")
    message("Catch: ", paste(collapse = ", ", Catchgears))
    message("Data: ", paste(collapse = ", ", Pstrat))
    
    if (sum(Pstrat %in% Catchgears) == 0) {
      stop(
        "No Pdata stratifications,\n",
        paste(Pstrat, collapse = ", "), "\n",
        "were found in catch columns,\n",
        paste(Catchgears, collapse = ", ")
      )
    } else {
      Pdata <- Pdata[Pdata[, "stratification"] %in% colnames(Catch), ]
      Catch <- Catch[, c(colnames(Catch)[yearcol], unique(Pdata[, "stratification"]))]
      if (verbose) {
        message("Data were truncated to just these stratifications:")
        message(
          "Catch: ",
          paste(sort(names(Catch)[-1]), collapse = ", ")
        )
        message(
          "Pdata: ",
          paste(sort(unique(Pdata$stratification)), collapse = ", ")
        )
      }
    }
  } # End if
  
  #### Expansion
  # Get summed sampled lbs per individual sample (trip, tow, sample number).
  
  # Convert Catch to lbs.
  Catch[, -1] <- measurements::conv_unit(
    to = "lbs",
    x = Catch[, -1], from = Units
  )
  
  Catch_long <- tidyr::pivot_longer(Catch, -tidyr::all_of(yearcol), 
                                    names_to = 'stratification', 
                                    values_to = 'catch')
  
  tows <- Pdata[!duplicated(Pdata$SAMPLE_NO), ]
  
  # Get the total lbs sampled by year and stratification
  strat <- c("fishyr", "stratification")
  tows <- tows %>%
    # ... are levels to aggregate over
    dplyr::group_by(fishyr, stratification) %>%
    dplyr::mutate(
      Sum_Sampled_Lbs = sum(Trip_Sampled_Lbs, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    # Matching rows in Pdata with Catch[, "Year"] and correct column in Catch
    dplyr::left_join(Catch_long, by = c('fishyr' = names(Catch_long)[yearcol],
                                        'stratification' = 'stratification'))
  
  # Find which trips don't have catch values associated with them
  trips_without_catch <- dplyr::filter(tows, is.na(catch))
  if (NROW(trips_without_catch) > 0) {
    NoCatch <- dplyr::group_by(
      .data = trips_without_catch,
      fishyr, stratification
    ) %>%
      dplyr::count(Sum_Sampled_Lbs)
    if (length(NoCatch) > 0 && verbose) {
      message(
        "No Catch was found for these rows in Pdata, where\n",
        "n is the number of rows with missing Catch info:"
      )
      print(NoCatch)
    } # End if
  }
  
  # Expansion is calculated by dividing the catch by the Sum_Sampled_Lbs.
  tows$EF2 <- tows$catch / tows$Sum_Sampled_Lbs
  tows$EF2[tows$EF2 < 1 | !is.finite(tows$EF2)] <- 1
  # Match EF2 to the larger dataset
  Pdata$Sum_Sampled_Lbs <- find.matching.rows(
    Pdata,
    tows, strat, strat, "Sum_Sampled_Lbs"
  )[[1]]
  Pdata$catch <- find.matching.rows(
    Pdata,
    tows, strat, strat, "catch"
  )[[1]]
  Pdata$Expansion_Factor_2 <- find.matching.rows(
    Pdata,
    tows, strat, strat, "EF2"
  )[[1]]
  
  NA_EF2 <- Pdata[is.na(Pdata$Expansion_Factor_2), ]
  nNA <- nrow(NA_EF2)
  Pdata$Expansion_Factor_2[is.na(Pdata$Expansion_Factor_2)] <- 1
  Pdata$Expansion_Factor_2 <- capValues(Pdata$Expansion_Factor_2, maxExp)
  Pdata[, "Final_Sample_Size_L"] <- capValues(
    Pdata$Expansion_Factor_1_L * Pdata$Expansion_Factor_2
  )
  Pdata[, "Final_Sample_Size_A"] <- capValues(
    Pdata$Expansion_Factor_1_A * Pdata$Expansion_Factor_2
  )
  
  #### Summary information
  if (verbose) {
    message(nNA, " NA Expansion_Factor_2 values replaced by 1.")
    message("Summary of Expansion_Factor_2")
    print(summary(Pdata$Expansion_Factor_2))
  }
  
  if (nNA > 0) {
    NA_EF2[, "FREQ"] <- 1
    if (!missing(savedir)) {
      grDevices::png(file.path(savedir, "PacFIN_exp2_NAreplace.png"))
      on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
      graphics::barplot(
        stats::xtabs(NA_EF2$FREQ ~ NA_EF2$state + NA_EF2$fishyr),
        col = grDevices::rainbow(3),
        legend.text = TRUE, xlab = "Year", ylab = "Samples",
        main = "Second-stage expansion values of NA replaced by 1"
      )
    } else {
      message("Specify savedir if you want a figure to show the NA Expansion_Factor_2 values replaced by 1.")
    }
  } # End if
  
  if (!missing(savedir)) {
    grDevices::png(file.path(savedir, "PacFIN_exp2_summarybyyear.png"))
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    graphics::boxplot(Pdata$Expansion_Factor_2 ~ Pdata$fishyr,
                      main = "", xlab = "Year", ylab = "Second-stage expansion factor"
    )
  }
  
  invisible(Pdata)
} # End function getExpansion_2
