#' Calculate weight-length relationship parameters
#'
#' Estimate parameters of the weight-length relationship for each
#' sex and all sexes combined, where the latter includes unsexed fish.
#'
#' @param data A data frame containing empirical weights and lengths
#' from sampled fish.
#' @template verbose
#'
#' @author Kelli Faye Johnson
#' @export
#' @return A data frame of weight-length parameters by sex.
#' Parameters A and B are in the appropriate units to input
#' into Stock Synthesis Wtlen_1_Fem and Wtlen_2_Fem, or
#' Wtlen_1_Mal and Wtlen_1_Mal, parameters in the
#' control file.
#'
getWLpars <- function(data, verbose = FALSE) {

  getline <- function(model){
    # function to get row of table of weight-length values
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

  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub("fish_weight", "weight", colnames(data))
  colnames(data) <- gsub("lengthcm", "length_cm", colnames(data))
  if (!any(c("sex", "weight", "length_cm") %in% colnames(data))) {
    warning("Necessary columns, i.e., sex, weight, and length_cm,",
      " are not available in the data set.")
    return(NULL)
  }

  dims <- dim(data)
  data <- data[
    !is.na(data[, "weight"]) &
    !is.na(data[, "length_cm"]), ]
  if (verbose) {
    message("Calculating the weight-length relationship from ",
      nrow(data), "\nfish because ", dims[1] - nrow(data),
      " fish did not have empirical weights and lengths.")
  }
 
  lmgrowth <- function(sex, thedata = data) {
    stats::lm(log(weight) ~ log(length_cm),
        data = thedata[thedata$sex %in% sex, ])
  }
  mresults <- lapply(list(
    females = "F",
    males = "M",
    all = c("F", "M", "U")),
    FUN = lmgrowth)
  if (verbose) {
    message("Weight-Length model results by SEX:")
    utils::capture.output(lapply(mresults, summary), type = "message")
  }
  WLresults <- t(vapply(mresults, FUN.VALUE = numeric(4), getline))

  return(WLresults)
}
