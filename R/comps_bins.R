#' Bin a vector of data into groups
#' 
#' Bin a vector of data into distinct groups, this is often helpful
#' for grouping ages or lengths into bin categories if every year or
#' centimeter is not its own bin.
#'
#' @template vector
#' @template breaks
#' @template includeplusgroup
#' @template returnclass
#' 
#' @export
#' @author Kelli Faye Johnson
#' @examples
#' comps_bins(1:8, breaks = c(-Inf, 3:5))
#' comps_bins(1:8, breaks = c(3:5), includeplusgroup = FALSE)
#' testthat::expect_equal(
#' comps_bins(1:8, breaks = c(-Inf, 3:5, Inf)),
#' comps_bins(1:8, breaks = c(-Inf, 3:5), includeplusgroup = TRUE)
#' )
#'
comps_bins <- function(vector, breaks,
  includeplusgroup = TRUE, returnclass = c("character", "numeric")) {
  returnclass <- match.arg(returnclass)
  breaks <- sort(utils::type.convert(breaks, as.is = TRUE))
  if (includeplusgroup && utils::tail(breaks, 1) != Inf) {
    breaks <- c(breaks, Inf)
  }
  out <- gsub(
    # Note that the hyphen must go last in the [] b/c it is
    # also used as a special character to represent ranges
    pattern = "[\\[\\(]([0-9\\Inf-]+),\\s*[0-9Inf\\)]+",
    replacement = "\\1",
    x = cut(vector, breaks = breaks,
      include.lowest = FALSE, right = FALSE)
    )
  out <- switch(returnclass,
    character = out,
    numeric = type.convert(out, as.is = TRUE))
  return(out)
}
