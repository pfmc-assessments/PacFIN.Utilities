##############################################################################
#
#' Utility function.
#' 
#' \subsection{\code{\link{Workflow}}}{
#' Used by \code{\link{find.matching.rows}}, not intended to be used otherwise.
#' }
#'
#' Converts each row in the input dataframe to a string, 
#' "pasting" the columns together.
#
#############################################################################
paste.col <- function(x) {

  # If it's just a vector, return each value as a string.

  if (is.null(dim(x))) {

    return(paste(as.character(x)))

  } # End if

  # Otherwise, it's a matrix.

  # Get the value of the first column in character form

  out <- paste(as.character(x[, 1]))

  # Add on each of the rest of the columns

  for (i in 2:ncol(x)) {

   out <- paste(out, as.character(x[, i]))

  } # End for

  return(out)

} # End paste.col
