#' Check  International North Pacific Fisheries Commission (INPFC) Areas
#'
#' Check that the INPFC areas are valid and within the areas to keep.
#' Note that many PacFIN records do not have an entry for \code{INPFC_AREA},
#' and thus, if you wish to keep these, then \code{""} will need to be added
#' to the vector supplied to the \code{"keep"} argument.
#'
#' @template data
#' @param keep A vector of types to keep. See \code{formals(checkINPFC)$keep}
#' for default values.
#' @return A vector of TRUE/FALSE values indicating which INPFC areas
#' are in those specified in the \code{keep} argument.
#' @export
#' @examples
#' checkINPFC(1:10, keep = ls_INPFC(area = "All"))
#' \dontshow{
#' testthat::expect_equal(
#'   c(TRUE, TRUE, TRUE, FALSE),
#'   checkINPFC(c("CAN", "CT", "GS", "AAA"), keep = ls_INPFC(area = "All"))
#' )
#' }

checkINPFC <- function(data, keep = ls_INPFC(area = "US")) {

  if (is.data.frame(data)) {
    data <- data[, "INPFC_AREA", drop = TRUE]
  }
  data[data == "COL"] <- "CL"
  data[data == "EU"] <- "EK"
  # Change areas to Vancouver, most are landed in Neah Bay
  data[data %in% c("JF", "VCN", "VUS", "WJ")] <- "VN"

  return(ifelse(data %in% keep, TRUE, FALSE))
}
