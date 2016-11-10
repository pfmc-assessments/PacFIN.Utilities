###################################################################
#
#' Create a vector of stratifications
#' 
#'
#' \code{Stratify} takes an input vector and list of values used to designate
#' the values returned in the strats vector.
#' 
#' \subsection{Workflow}{
#' \code{Stratify} is intended to add levels for stratification before
#' running \code{\link{getComps}}.
#' }
#'
#'
#' @param inVector Input vector of values.  May be numeric or character.
#' @param splits List of values to use to stratify the input vector.  May be
#'        character literals or numeric ranges.
#' @param Nnames  Vector the length of \code{splits} naming the strata.
#' @param numeric A logical value, where if \code{TRUE} then
#'   \code{findInterval} will be used and stratum 0 will be assigned to values
#'   smaller than the first value in splits.
#'
#' @details 
#' Any values not given in \code{splits} will be assigned to stratum 0.
#' This function can be used to designate fleet or for stratification
#' based on depth or INPFC area.
#' 
#' @examples
#' Pdata$use_depth = Stratify( Pdata$DEPTH_AVG, splits=list(0, 50, 100, 250, 500), numeric=T)
#' 
#' Pdata$my_fleets = Stratify( Pdata$GEAR,
#'            splits=list("GFS", c("MDT","TB"), "TR", c("LGL","FTS")),
#'            Nnames=list(“G1”,”G2”,”G3”,”G4”) )
#'
#' @return Returns a vector the length of the input vector, with either numeric 
#' levels indicating the stratification of the input, or with Nnames assigning
#' levels of the input vector.
#' 
#' @author Andi Stephens

Stratify = function ( inVector=NULL, splits=NULL, Nnames=NULL, numeric=F ) {
  

  if (! is.list(splits)) { stop("splits must be a list") }
  
  if (is.list(Nnames)) {
    
    Nnames = unlist(Nnames)
    
  } # End if
  
  stratified = rep(0, length(inVector))

  if ( !is.null(Nnames) ) {

    if ( length(Nnames) != length(splits) ) {

      cat("Nnames vector is not the same length as splits")

      return(NA)

    } # End if


  } # End if

  if (!numeric) {

    inVector = as.character(inVector)

    for ( i in 1:length(splits) ) {

      splitby = as.character(splits[[i]])

      if (is.null(Nnames)) {

        stratified[inVector %in% splitby] = i

      } else {

       stratified[inVector %in% splitby] = Nnames[i]

      } # End if

    } # End for

  } else {
    
      
    names(inVector) = seq(1,length(inVector))
    inVector = sort(inVector)
   
    savedOrder = as.numeric(names(inVector))
    
    stratified = findInterval(inVector, unlist(splits))
    stratified = stratified[ order(savedOrder) ]
    
    if (length(Nnames) > 0) {
      
      namelist = Nnames[stratified]
      stratified = namelist
      
    } # End if
  } # End if-else

  return(stratified)

} # End function Stratify
