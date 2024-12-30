#' Add a column to `Pdata` for season.
#'
#' Several seasonal schemes are available, including Petrale seasons
#' (1 = winter months, 2 else).
#' Most assessments won't require `getSeason` because it is included
#' in [cleanPacFIN]. If a specialized season structure is required,
#' `getSeason` should be run immediately after [cleanPacFIN].
#'
#' @inheritParams cleanPacFIN
#' @param season_type Specify a `numeric` value for season type.
#' If negative then all months will be assigned to season `1`.
#' If `0` then seasons will be assigned from `Pdata$SAMPLE_MONTH`,
#' where each month is a unique season.
#' If `1` then seasons are assigned according to methods used for Petrale,
#' where winter months (`c(11:12, 1:2)`) are season `1` and
#' the remaining months (summer) are assigned to season `2`.
#' Please contact the package maintainer should you wish to include an
#' additional seasonal scheme.
#' @param yearUp Used to provide a list of months (i.e., `1:12`)
#' for which to adjust the year (`Pdata$fishyr`) up. For example,
#' if winter months belong to the following year then use `yearUp = 11:12`.
#' @param yearDown Used to provide a list of months (i.e., `1:12`)
#' for which to adjust the year (`Pdata$fishyr`) down. For example,
#' if winter months belong to the previous year then use `yearUp = 1:2`.
#' @param plotResults A logical value specifying if plots should or should not
#' be created and shown in the console.
#' @inheritParams cleanPacFIN
#'
#' @return An additional column `season` is added to `Pdata`.
#' No columns are modified.
#' @export
#' @author Andi Stephens
#' @examples
#' test <- getSeason(
#'   data.frame(SAMPLE_MONTH = 1:12, fishyr = rep(1:2, each = 6)),
#'   verbose = TRUE
#' )
#' testthat::expect_true(all(test[, "season"] == 1))
#' test <- getSeason(Pdata = test, season_type = 1, yearUp = 12)
#' testthat::expect_equivalent(test[test[, "fishyr"] == 3, "season"], 1)
#'
getSeason <- function(Pdata,
                      season_type = -1,
                      yearUp = NULL,
                      yearDown = NULL,
                      plotResults = FALSE,
                      verbose = TRUE) {
  if (season_type < 0) {
    Pdata$season <- 1
  }

  if (season_type == 0) {
    if (verbose) {
      message("Assigning season from SAMPLE_MONTH.")
    }

    Pdata[, "season"] <- utils::type.convert(as.is = TRUE, Pdata$SAMPLE_MONTH)
  } # End if

  # Petrale seasons

  if (season_type == 1) {
    if (verbose) {
      message("Assigning seasons for Petrale; winter == 1, summer == 2.")
    }

    Pdata[, "season"] <- ifelse(Pdata[, "SAMPLE_MONTH"] %in% c(11:12, 1:2),
      1, 2
    )
  } # End if Petrale

  if (!is.null(yearUp)) {
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] <-
      Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] + 1

    if (verbose) {
      message(
        "Incremented fishyr for months ",
        paste(yearUp, collapse = ", "), "to the next year."
      )
    }
  } # End if yearUp

  if (!is.null(yearDown)) {
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] <-
      Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] - 1

    if (verbose) {
      message(
        "Decremented fishyr for months ",
        paste(yearDown, collapse = ", "), "to the previous year."
      )
    }
  } # End if yearDown

  if (plotResults) {
    tmp <- table(Pdata[, c("season", "SAMPLE_YEAR")])
    graphics::barplot(tmp,
      col = grDevices::rainbow(NROW(tmp)),
      legend.text = paste("Season", rownames(tmp)),
      main = unique(Pdata$SPID), xlab = "Year", ylab = "Count",
      bty = "n"
    )
  } # End if plotResults

  return(Pdata)
}
