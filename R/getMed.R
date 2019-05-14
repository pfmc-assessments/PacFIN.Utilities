#' Calculate Category-Specific Medians
#' 
#' Compute medians by categories with \code{NA} values omitted.
#' This eliminates the need to match results back to the 
#' original data set using merge or indexing functions. 
#' 
#' @param x A vector of values you want split by categories listed
#' in \code{...}. Number of values in \code{x} must be the same
#' length as each vector supplied in \code{...}.
#' @param ... Categories that you want to use to split the data.
#' Any number can be supplied. 
#' 
#' @return A vector of medians with one entry for each original
#' entry because the results are duplicated for each entry that
#' went into the calculation.
#' @author Kelli Faye Johnson
#' 
#' @examples
#' data <- data.frame(
#'   "cat1" = rep(c("yes", "no"), each = 10),
#'   "cat2" = rep(1:5, each = 4),
#'   "val" = rnorm(20))
#' PacFIN.Utilities:::getMed(x = data[, "val"], 
#'   data[, "cat1"], data[, "cat2"])
#' 
getMed <- function(x, ...) {
  all <- list(...)
  temp <- all
  out <- matrix(NA, ncol = length(all) + 1, nrow = length(x))
  for (ii in rev(seq_along(all))) {
    g <- interaction(temp)
    temp <- temp[-ii]
    out[, ii] <- unsplit(lapply(split(x, g),
      FUN = median, na.rm = TRUE), g) 
  }
  out[, ncol(out)] <- apply(out, 1, 
    function(x) ifelse(all(is.na(x)), NA, tail(x[!is.na(x)], 1)))
  colnames(out) <- c(letters[seq_along(all)], "median")
  return(as.data.frame(out))
}
