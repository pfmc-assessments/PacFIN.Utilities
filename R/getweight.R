#' Calculate Weight From Parameters
#' 
#' Estimate fish weights from sex-specific, or not, parameters
#' estimated, or formated the same way, as that returned from
#' \code{\link{getWLpars}}.
#' 
#' @param length A vector of fish lengths in mm.
#' @param sex A vector of sexes for each entry in \code{length}.
#' Entries must be one of the following: 'U', 'F', 'M', or 'H'.
#' @param pars A data frame of parameters for the weight-length
#' relationship as determined from empirical data. The data frame
#' must have columns of \code{'A'} and \code{'B'}, as well as 
#' rows named \code{'U'}, at a minimum, \code{'F'}, and \code{'M'}.
#' The output from \code{\link{getWLpars}} is formatted correctly as is. 
#' 
#' @export
#' @author Kelli Faye Johnson
#' @return A vector of weights determined from lengths and weight-length
#' parameters input to the function. 
#' Weights are in pounds. 
#' 
getweight <- function(length, sex, pars) {
    # a * (length / 10)^b * 2.20462 [length = cm; weight = kg]
  if (length(length) != length(sex)) stop("The vectors, length and",
    " sex, must be equal in length.")
  if (is.matrix(pars)) pars <- data.frame(pars)
  pars["H", ] <- pars["all", ]
  if (!"females" %in% rownames(pars)) pars["F", ] <- pars["all", ]
  if (!"males" %in% rownames(pars)) pars["M", ] <- pars["all", ]
  pars$SEX <- rownames(pars)
  pars$SEX <- gsub("(^f|^m).+", "\\U\\1", pars$SEX, perl = TRUE)
  pars$SEX <- gsub("all", "U", pars$SEX)
  calcweight <- (pars[match(sex, pars[, "SEX"]), "A"] * 
    (length / 10)^(pars[match(sex, pars[, "SEX"]), "B"])) * 2.20462
  return(calcweight) # in pounds
  }
  