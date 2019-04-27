#' Calculate Weight-Length Relationship
#' 
#' Estimate parameters of the weight-length relationship for each
#' sex and all sexes combined, where the latter includes unsexed fish.
#' 
#' todo: Return model diagnostics if verbose. This will entail
#' saving each model in an object and then extracting the pars
#' via getline rather than doing it all in one step.
#' 
#' @param data A data frame containing empircal weights and lengths
#' from sampled fish.
#' @template verbose
#' 
#' @import stats
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
    out <- as.numeric(c(
      "Amedian" = Amed, 
      "Asd" = sdres, 
      "A" = Amean, 
      "B" = B))
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
  WLresults <- t(sapply(list(
    "females" = "F", 
    "males" = "M", 
    "all" = c("F", "M", "U")), 
    function(x) { setNames(
      getline(lm(log(weight) ~ log(length_cm), 
        data = data[data$sex %in% x, ])),
      c("median_intercept", "SD", "A", "B"))
    }))

  return(WLresults)
}
