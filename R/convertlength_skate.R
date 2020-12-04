#' Convert Disc and Interspiracular Width to Length
#'
#' Convert from disc and interspiracular width to length for skates, 
#' Conversion parameters were derived from 
#' West Coast Groundfish Bottom Trawl Survey data.
#' 
#' @template Pdata
#' @param returntype A character value from the list supplied that specifies
#' the data you want returned. 
#' \itemize{
#'   \item {\code{"all"}} {all lengths with the new estimates replacing 
#' the input lengths} 
#'   \item {\code{"estimated"}} {only the estimat lengths 
#' with all other lengths as NA such that the
#' returned vector is the same length and the number of rows of the input data
#' frame.}
#' }
#' @export
#' @author Kelli Faye Johnson and Vladlena Gertseva
#' @return A vector of lengths. 
#' See \code{returntype} for detailed information on what can be returned.
#' 
convertlength_skate <- function(Pdata, returntype = c("all", "estimated")) {
  matchcol <- function(data) {
    apply(data[, c("SEX", "FISH_LENGTH_TYPE")], MARGIN = 1, 
      FUN = paste, collapse = "_")
  }
  returntype <- match.arg(returntype, several.ok = FALSE)
  
  # Conversion parameters
  discpar <- data.frame(
    "SEX" = rep(c("F", "M", "U"), 2),
    "FISH_LENGTH_TYPE" = c(rep("A", 3), rep("R", 3)),
    "multiply" = c(c(1.4021, 1.4058, 1.4044), c(12.538, 13.172, 12.538)),
    "add" = c(c(9.117, 5.2334, 7.005), c(70.48, 35.21, 70.48)))
  discpar[, "match"] <- matchcol(discpar)
  matches <- match(matchcol(Pdata), discpar[, "match"])
  
  est <- Pdata[, "FISH_LENGTH"] * discpar[matches, "multiply"] + 
    discpar[matches, "add"]
  
  returned <- switch(returntype,
    all = ifelse(Pdata[, "FISH_LENGTH_TYPE"] %in% c("A", "R"), 
      est, Pdata[, "FISH_LENGTH"]),
    estimated = est)
  return(returned)
}
