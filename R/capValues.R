#' Reset all values greater than a maximum specified value.
#'
#' \code{capValues} takes a vector and will return a vector where all
#' values greater than some specified maximum are reset at that maximum
#' value. The maximum (\code{maxVal}) may be specified as either a
#' quantile or a true numeric value.
#'
#' @param DataCol A vector of numeric values.
#' @param maxVal A numeric value specifying the quantile or value to cap
#'   all data at. The default value is \code{0.95}, which caps all values
#'   in \code{DataCol} at the 95 percent quantile. If a numeric value
#'   greater than one is specified then all values in \code{DataCol} greater
#'   than the specified value will be reset to \code{maxVal}. If \code{maxVal}
#'   is less than one it assumes a quantile is specified.
#' @return A vector of numeric values where no value is greater than the
#'   maximum specified by \code{maxVal}.
#' @example
#' x <- 1:10
#' y <- capValues(x, 5)
#' z <- capValues(x, 0.75)
#' rbind(x, y, z)
#' @author Andi Stephens

capValues = function( DataCol, maxVal=0.95 ) {

  if ( maxVal > 1 ) {

    max.val = maxVal
    cat("\nMaximum value capped at", max.val, "\n\n")

  } else {

    max.val = quantile(DataCol, maxVal, na.rm=T)
    cat("\nMaximum expansion capped at", maxVal, "quantile:", max.val, "\n\n")

  } # End if-else

  DataCol[DataCol > max.val] = max.val

  return(DataCol)

} # End function capValues
