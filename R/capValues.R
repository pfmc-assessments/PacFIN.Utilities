##########################################################################
#
#' Decrease all values greater than a maximum specified value.
#'
#' \code{capValues} takes a numeric vector and returns a vector in which all
#' values greater than a specified maximum are reset to that maximum
#' value. 
#' 
#' 
# \if{html}{\figure{workflow.png}{\options: alt=\"Figure: workspace_id.png\"}}
#'
#\if{latex}{\figure{workflow.png}{\options: alt=\"Figure: workspace_id.png\"}}
#'
#' @details The maximum may be specified either as a
#' quantile or as a number. 
#' 
#' If \code{maxVal} is less than one, it is
#' interpreted as a quantile, otherwise it is interpreted as the maximum
#' value to return.  
#' 
#' The default value is \code{0.95}, which caps all
#' values in the input at the 95 percent quantile.
#'  
#'
#' @param DataCol A vector of numeric values.
#' @param maxVal A numeric value specifying the maximum value or quantile
#'   at which to cap all data. 
#' @return A vector of numeric values where no value is greater than the
#'   maximum specified by \code{maxVal}.
#'   
#' @examples
#' x = seq(1,10)
#' y <- capValues(x, 0.75)
#' z <- capValues(x, 5)
#' rbind(x, y, z)
#'
#' @author Andi Stephens
#' 
###########################################################################

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
