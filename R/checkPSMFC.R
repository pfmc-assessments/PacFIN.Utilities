#' Check PSMFC Areas
#'
#' Check that the PSMFC areas are valid and within the areas to keep.
#' Note that many PacFIN records do not have an entry for \code{PSMFC_AREA},
#' and thus, if you wish to keep these, then \code{""} will need to be added
#' to the vector supplied to the \code{"keep"} argument.
#'
#' @template data
#' @param keep A vector of types to keep. See \code{formals(checkPSMFC)$keep}
#' for default values.
#' @return A vector of TRUE/FALSE values indicating which PSMFC areas
#' are in those specified in the \code{keep} argument.
#' @export
checkPSMFC <- function(data, keep) {

  all <- rep(0, nrow(data))
  if (is.data.frame(data)) {
    data <- data[, "PSMFC_AREA", drop = TRUE]
  }

  match <- grep(paste(keep, collapse="|"), data)
  all[match] = 1

  return(ifelse(all == 1, TRUE, FALSE))
}
