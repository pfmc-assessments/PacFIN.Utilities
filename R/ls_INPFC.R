#' List International North Pacific Management Council (INPFC) Areas
#'
#' List (ls) INPFC areas of interest.
#'
#' @param area A valid group that you want to look up. Available options for this
#' argument are listed in the function call. The function is not vectorized,
#' i.e., only allows a single input value.
#'
#' @export
#' @return A vector of character values within the group specified.
#' @examples
#' ls_INPFC(area = "US")
#'
ls_INPFC <- function(area = c("ALL", "US")) {
  area <- toupper(area)
  area <- ifelse(area == "USA", "US", area)
  area <- match.arg(area, several.ok = FALSE)
  USvector <- c("VUS", "CL", "COL", "NC", "SC",
    "CALCOM", "CP", "EK", "EU", "MT", "VN")

  out <- switch(area,
    "ALL" = INPFCTable[, "INPFC", drop = TRUE],
    "US" = USvector,
    "invalid area"
  )
  return(out)
}
