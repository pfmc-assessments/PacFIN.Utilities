#' Check Argument Comps
#'
#' Check and select the appropriate input value for the Comps
#' parameter used in multiple functions within \pkg{PacFIN.Utilities}.
#'
#' @template Comps
#' @author Kelli F. Johnson
#' @return A single character value.
#'
checkparam.Comps <- function(Comps = c("LEN", "AGE", "AAL", "NA")) {
  Comps <- toupper(substr(Comps, 1, 3))
  return(match.arg(Comps, several.ok = FALSE))
}
