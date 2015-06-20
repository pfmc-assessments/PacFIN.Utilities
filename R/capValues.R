##############################################################################
#
# capValues takes a data column and a capping factor, which may be either
# a quantile (less than 1), or a value (greater than 1), and reduces outsize
# values in the data to the value given, or to the value of the dataset at
# that quantile.
#
# For example, the default 0.95 quantile in a dataset with maximum value 10
# might be 8.5.  Then all values greater than 8.5 will be reduced to 8.5.
#
# If a value is given, for example 9.3, then all values greater than 9.3
# will be reset to 9.3.
#
# A copy of the input data column with the modified values is returned.
#
##############################################################################

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
