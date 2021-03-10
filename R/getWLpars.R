#' Calculate weight-length relationship parameters
#'
#' Estimate parameters of the weight-length relationship for each
#' sex and all sexes combined, where the latter includes unsexed fish.
#'
#' @param data A data frame containing empirical weights and lengths
#' from sampled fish.
#' Sexes should be available in the column `sex` or `SEX`.
#' @param col.length A numeric or character value specifying the column
#' to use in `data` for length information. These lengths are assumed to
#' be in centimeters. The default value is `lengthcm`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' @param col.weight A numeric or character value specifying the column
#' to use in `data` for weight information. These weights are assumed to
#' be in kilograms The default value is `weightkg`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' Using kilograms is the default because Stock Synthesis assumes the
#' weight-length parameters are calculated using centimeters and kilograms.
#' The reported values are easily scaled to give you results in grams if
#' you wish to have more standard parameter estimates.
#' @template verbose
#'
#' @author Kelli Faye Johnson
#' @export
#' @return A data frame of weight-length parameters by sex.
#' Parameters A and B are in the appropriate units to input
#' into Stock Synthesis Wtlen_1_Fem and Wtlen_2_Fem, or
#' Wtlen_1_Mal and Wtlen_1_Mal, parameters in the
#' control file. Values of `NA` are returned for models that
#' did not have enough data to properly estimate the parameters.
#' This will happen when there are no females in your data set,
#' for example.
#'
getWLpars <- function(
  data,
  col.length = "lengthcm",
  col.weight = "weightkg",
  verbose = FALSE
  ) {

  getline <- function(model){
    # function to get row of table of weight-length values
    if (is.null(model)) {
      return(c(median_intercept = NA, SD = NA, A = NA, B = NA))
    }
    Amed <- exp(model$coefficients[1])
    B <- model$coefficients[2]
    sdres <- sd(model$residuals)
    Amean <- Amed*exp(0.5*sdres^2)
    out <- utils::type.convert(as.is = TRUE, c(
      "median_intercept" = Amed,
      "SD" = sdres,
      "A" = Amean,
      "B" = B))
    names(out) <- c("median_intercept", "SD", "A", "B")
    return(out)
  }

  lmgrowth <- function(sex, thedata = data, verbose = verbose) {
    data.model <- thedata[thedata$sex %in% sex, ]
    out <- tryCatch(
      {
        stats::lm(log(weight) ~ log(length_cm),
          data = data.model)
      },
      error = function(cond) {
        if (verbose) {
          message("Too few data points for sex = ",
            "c(", paste0("'", sex, "'", collapse = ", "), ")")
        }
        return(NULL)
      }
    )
    return(out)
  }

  col.length <- tolower(col.length)
  col.weight <- tolower(col.weight)
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub(col.weight, "weight", colnames(data))
  colnames(data) <- gsub(col.length, "length_cm", colnames(data))
  col.length <- "length_cm"
  col.weight <- "weight"
  stopifnotcolumn(data = data, string = col.length)
  stopifnotcolumn(data = data, string = col.weight)
  stopifnotcolumn(data = data, string = "sex")

  dims <- dim(data)
  data <- data[
    !is.na(data[[col.weight]]) &
    !is.na(data[[col.length]]), ]
  if (verbose) {
    message("Calculating the weight-length relationship from ",
      nrow(data), "\nfish because ", dims[1] - nrow(data),
      " fish did not have empirical weights and lengths.")
  }

  mresults <- lapply(list(
    females = "F",
    males = "M",
    all = unique(data[, "sex"])),
    FUN = lmgrowth)
  if (verbose) {
    message("Weight-Length model results by SEX:")
    utils::capture.output(lapply(mresults, summary), type = "message")
  }
  WLresults <- t(vapply(mresults, FUN.VALUE = numeric(4), getline))

  return(WLresults)
}
