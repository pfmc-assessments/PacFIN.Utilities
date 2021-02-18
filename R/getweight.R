#' Calculate weight from length and the weight-length relationship
#'
#' Estimate fish weights from potentially sex-specific weight-length
#' relationship parameters returned from [getWLpars].
#'
#' @param length A vector of fish lengths in mm.
#' @param sex A vector of sexes for each entry in \code{length}.
#' Entries must be one of the following: 'U', 'F', 'M', or 'H'.
#' @param pars A data frame of parameters for the weight-length
#' relationship as determined from empirical data. The data frame
#' must have columns of \code{'A'} and \code{'B'}, as well as
#' rows named \code{'U'}, at a minimum, \code{'F'}, and \code{'M'}.
#' The output from \code{\link{getWLpars}} is formatted correctly as is.
#' @template unit.out
#' @param weight A vector of fish weights, where the units for each
#' measurement are specified in `unit.in`.
#' @param unit.in A vector of units for each measurement in `weight`.
#' Options include `KG`, `G`, `LB`, `UNK`. Where, the latter leads to
#' the assumption that your units are in grams and will be converted
#' as such.
#'
#' @export
#' @author Kelli Faye Johnson
#' @return A vector of measurements in the unit specified using
#' `unit.out`. If weights were provided, then the weights are also
#' the output. If lengths were provided, then they are converted
#' to weights. Where, weights are determined from lengths and weight-length
#' parameters input to the function.
#' Weights are in the same units used to calculate things, i.e., kg.
#' @examples
#' data(XMPL.BDS)
#' lfish <- 1:100; sfish <- rep("U", length(lfish))
#' ans <- getweight(length = lfish, sex = sfish,
#'   pars = getWLpars(cleanPacFIN(XMPL.BDS, CLEAN = FALSE,
#'   keep_length_type = "F")))
#' testthat::expect_equivalent(ans[1], 3.625679e-08)
#' \dontrun{
#' plotWL(length = lfish, sex = sfish,
#' weight = mapply(rnorm, mean = ans, MoreArgs = list(n = 1, sd = 0.001)),
#' weight.calc = ans
#' )
#' }
#'
getweight <- function(
  length,
  sex,
  pars,
  unit.out = c("lb", "kg"),
  weight,
  unit.in) {

  if (!missing(weight)) {
    transformweight <- weight * mapply(switch, unit.in,
        MoreArgs = list(
          G = 0.00220462,
          KG = 2.20462,
          UNK = 0.00220462,
          0.00220462)
        )
    if (unit.out == "kg") {
      transformweight <- transformweight * 0.453592
    }
    return(transformweight)
  }

  #### Original equation
  # a * (length / 10)^b * 2.20462 [length = cm; weight = kg]

  #### Checks
  unit.out <- match.arg(unit.out, several.ok = FALSE)
  stopifnot(all(sex %in% c(NA, "U", "F", "M", "H")))
  if (length(length) != length(sex)) stop("The vectors, length and",
    " sex, must be equal in length.")
  if (is.matrix(pars)) pars <- data.frame(pars)
  if ((!"H" %in% row.names(pars)) & "H" %in% sex & "all" %in% row.names(pars)) {
    pars["H", ] <- pars["all", ]
  }
  if ((!"H" %in% row.names(pars)) & "H" %in% sex) {
    stop("H is in the sex vector but no parameters in pars are available.")
  }
  if (!"females" %in% rownames(pars)) pars["F", ] <- pars["all", ]
  if (!"males" %in% rownames(pars)) pars["M", ] <- pars["all", ]
  pars$SEX <- rownames(pars)
  pars$SEX <- gsub("(^f|^m).+", "\\U\\1", pars$SEX, perl = TRUE)
  pars$SEX <- gsub("all", "U", pars$SEX)

  #### Calculate weight assuming length is in mm
  calcweight <- (pars[match(sex, pars[, "SEX"]), "A"] *
    (length / 10)^(pars[match(sex, pars[, "SEX"]), "B"]))
  if (unit.out == "lb") {
    calcweight <- calcweight * 2.20462
  }

  #### return
  return(calcweight)
  }
