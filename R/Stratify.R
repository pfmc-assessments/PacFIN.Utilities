##############################################################################
#
# Stratify takes an input vector and list of values used to designate
# the values returned in the strats vector.
#
# Any values not given in splits will be assigned to stratum 0.
#
# If numeric=T, this defaults to a call to findInterval, and stratum 0
# will be assigned to values smaller than the first value in splits.
#
# Note that this can be used to designate fleet or for stratification
# based on depth or INPFC area.
#
##############################################################################

Stratify = function ( inVector=NULL, splits=NULL, names=NULL, numeric=F ) {

  stratified = rep(0, length(inVector))

  if ( !is.null(names) ) {

    if ( length(names) != length(splits) ) {

      cat("Names vector is not the same length as splits")

      return(NA)

    } # End if


  } # End if

  if (!numeric) {

    inVector = as.character(inVector)

    for ( i in 1:length(splits) ) {

      splitby = as.character(splits[[i]])

      if (is.null(names)) {

        stratified[inVector %in% splitby] = i

      } else {

       stratified[inVector %in% splitby] = names[i]

      } # End if

    } # End for

  } else {

    stratfied = findInterval(inVector, splits)

  } # End if-else

  return(stratified)

} # End function Stratify
