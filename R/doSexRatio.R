############################################################################
#
#' Assign gender for unsexed fish in compiled Comp data.
#' 
#' @description
#' doSexRatio determines gender for unsexed fish in Age or Length comps.
#' 
#' \subsection{Workflow}{
#' Sex ratios are assigned after the data is stratified by \code{\link{getComps}}
#' and before running \code{\link{writeComps}}.
#' 
#'   \strong{If \code{\link{writeComps}} is run without running \code{doSexRatio}, all unsexed
#'   fish will be discarded.}
#' }
#'
#' @export
#'
#' @param CompData data that have already been aggregated by \code{getComps}.
#' @param findRatio logical.  Default: FALSE.  If TRUE, use the observed per-stratum ratio
#' of F:M to assign the unsexed fish.
#' @param Rvector sex ratios to apply to the given Bins, or a single ratio applie to all.
#' @param Bins Bins (age or length) corresponding to Rvector
#' @param ratioU the ratio to apply to fish less than maxsizeU
#' @param maxsizeU the size below which the sex ratio is assumed ratioU.
#' @param GTsizeU the size above which the ratio is assumed to be 1.0 (big mamas).
#'
#' @details
#' Sex ratios may be assigned in one of four different ways.
#' 
#' The default is to use a single-valued Rvector (0.5 or user-set) as the ratio to use for all
#' unsexed fish.
#' \itemize{
#' \item{\code{C1 = doSexRatio(CompData)}}
#' }
#' 
#' If findRatio is TRUE, then the observed per-stratum ratio of females to males is used for 
#' the usexed fish in each stratum, and no other parameters apply.
#' \itemize{
#' \item{\code{C1 = doSexRatio(Compdata, findRatio=T)}}
#' }
#' 
#' If maxsizeU is set, then all fish below that size  will be set to the value in
#' ratioU.  If GTsizeU is set, then all unsexed fish above that size will be female.
#' 
#' In this example, the ratio for fish less than 3 is 0.5, for fish greater than 12 is 1,
#' and for the fish in between is 0.6.
#' 
#' \itemize{
#' \item{\code{C1 = doSexRatio(Compdata, Rvector=0.6, ratioU=0.5, maxsizeU=3, GTsizeU=12)}
#' }}
#' 
#' If bins are used, they are assumed to be the length or age bins (for AGE comps) corresponding
#' to the vector of ratios (unused for a single ratio).  If Age-at-Length comps, the bins are
#' Lengths.
#'
#' In this example, lengths (ages) below the lowest Bin will fall into the
#' lowest bin; i.e., lengths from 0-15 will have ratio .5, and fish above 36 will have ratio 
#' 0.7 applied.
#' 
#' \itemize{
#' \item{\code{C1 = doSexRatio(Compdata, Rvector=c(.5, .6, .7), Bins=c(10, 15, 36))}
#' }}
#'  
#' @return
#' 
#' Returns Comp data with unsexed fish now assigned sexes (the values for males and
#' females in the comps have increased), however the original columns
#' for unsexed fish remain unchanged.  The function that writes out the comps 
#' \code{\link{writeComps}} sets these to zero.
#' 
#' @author
#' 
#' Andi Stephens, Kelli Johnson
#' 
#' @seealso
#' \code{\link{getComps}}, \code{\link{writeComps}}
#'
#############################################################################

doSexRatio = function( CompData, findRatio=FALSE, Rvector=0.5, Bins=NULL,
                       ratioU=.5, maxsizeU=0, GTsizeU=Inf ) {


  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!

  AGE_FLAG = FALSE

  if ( length(CompData$lengthcm) == 0 ) {

    index = which(names(CompData) == "age")
    names(CompData)[index] = "lengthcm"

    AGE_FLAG = TRUE

  } # End if

  # Fix arithmetic

  CompList = c("male","msamps","mtows","female","fsamps","ftows","unsexed","usamps","ONLY_U_TOWS","alltows")
  tmp = CompData[,CompList]
  tmp[is.na(tmp)] = 0
  CompData[,CompList] = tmp
  
  if ( GTsizeU != 0 & maxsizeU != 0 ) {
    
    if ( length(Rvector) > 1 ) { stop("Rvector expected to be a single ratio if you use GTsizeU") }
    
    # Create vectors from the appropriate values.
    
    Rvector = c(ratioU, Rvector, 1)
    Bins = c(0, maxsizeU, GTsizeU)
    
  }

  if ( findRatio ) {

    cat("\nApplying per-stratum sex ratio.\n\n")

    getRatio = function( x, y ) {

      tmp = ( x / ( x + y ))
      tmp[x == 0 & y == 0] = 0.5
      tmp[x != 0 & y == 0] = 1

      return(tmp)

    } # End if findRatio

    # KFJ: If bins are supplied, only use the ratio for those bins
    # that have a user supplied value of NA, this way a user can give
    # values for lengths below a certain size and then the calculated
    # ratio can be used for lengths greater than, or something along those
    # lines.

    # Also instead of using your own function you can use
    # calcRatio <- prop.table(CompData[, c("female", "male")], margin = 1)[, 1]

    # Though I think a good addition would be to look at neighboring bins
    # for a ratio rather than just assigning a value of 0.5 when there are
    # no observations in a given bin. Maybe even use a hierarchy of 2 nearest
    # bins, then same size but different year? Not sure on the best protocol here.
    # Could look to https://github.com/nwfsc-assess/nwfscSurvey/blob/master/R/SS3LF.fn.R
    # code for the sexRatio protocol to keep it consistent.

    calcRatio = getRatio(CompData$female, CompData$male)

    # If there are only unsexed in a stratum, need to replace NAs

    calcRatio[is.na(calcRatio)] = 0.5

    cat("\nSummary of sex ratios observed per stratum:\n\n")

    print(summary(calcRatio))

    # Now combine the calculated ratio with a user-supplied ratio

    if (length(Rvector) > 1) {

      Ratio <- Rvector[match(CompData$lengthcm, Bins)]
      Ratio[is.na(Ratio)] <- calcRatio[is.na(Ratio)]

    } else {

      # Use the calculated ratio for all bins
      Ratio <- calcRatio

    } # End if-else

    for ( i in 1:nrow(CompData) ) {

      CompData$female[i] = CompData$female[i] + CompData$unsexed[i] * Ratio[i]

      CompData$fsamps[i] = CompData$fsamps[i] + CompData$usamps[i] * Ratio[i]

      CompData$ftows[i] = CompData$ftows[i] + CompData$ONLY_U_TOWS[i] * Ratio[i]

      CompData$male[i] = CompData$male[i] + CompData$unsexed[i] * ( 1 - Ratio[i] )

      CompData$msamps[i] = CompData$msamps[i] + CompData$usamps[i] * ( 1 - Ratio[i] )

      CompData$mtows[i] = CompData$mtows[i] + CompData$ONLY_U_TOWS[i] * ( 1 - Ratio[i] )

    } # End for

    # If debugging:  CompData = cbind(CompData, Ratio)

    return(CompData)

  } # End if findRatio

  if ( length(Rvector) > 1 ) {

    if ( length(Bins) != length(Rvector) ) { stop("Rvector and Bins are not the same length") }

    tmp = paste(Rvector, collapse = ",")

    cat("\nApplying sex ratio:", tmp, "to numbers, samples and tows\n\n")

    # Recode lengths to correspond to bins given

    lens = findInterval(CompData$length, Bins, rightmost.closed=T)

    # Should be unnecessary

    if ( min(lens) == 0 ) {

      lens[lens == 0] = 1
      cat("Lengths/ages less than the lowest bin will be treated as belonging
          to that bin")

    } # End if

    for ( i in sort(unique(lens)) ) {

      CompData$female[lens == i] = CompData$female[lens == i] +
                                   CompData$unsexed[lens == i] *
                                   Rvector[i]

      CompData$fsamps[lens == i] = CompData$fsamps[lens == i] +
                                   CompData$usamps[lens == i] *
                                   Rvector[i]

      CompData$ftows[lens == i] = CompData$ftows[lens == i] +
                                  CompData$ONLY_U_TOWS[lens == i] *
                                  Rvector[i]

      CompData$male[lens == i] = CompData$male[lens == i] +
                                 CompData$unsexed[lens == i] *
                                 (1 - Rvector[i])

      CompData$msamps[lens == i] = CompData$msamps[lens == i] +
                                   CompData$usamps[lens == i] *
                                   (1 - Rvector[i])

      CompData$mtows[lens == i] = CompData$mtows[lens == i] +
                                  CompData$ONLY_U_TOWS[lens == i] *
                                  (1 - Rvector[i])

    } # End for

  } else {

    tmp = paste(Rvector, collapse = ",")
    cat("\nApplying sex ratio:", tmp, "to numbers, samples and tows\n\n")

    # apply a single ratio over all lengths

    CompData$female = CompData$female + Rvector * CompData$unsexed
    CompData$fsamps = CompData$fsamps + Rvector * CompData$usamps
    CompData$ftows = CompData$ftows + Rvector * CompData$ONLY_U_TOWS

    CompData$male = CompData$male + (1 - Rvector) * CompData$unsexed
    CompData$msamps = CompData$msamps + (1 - Rvector) * CompData$usamps
    CompData$mtows = CompData$mtows + (1 - Rvector) * CompData$ONLY_U_TOWS

  } # End if-else

  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!

  if (AGE_FLAG) {

    index = which(names(CompData) == "lengthcm")
    names(CompData)[index] = "age"

  } # End if

  cat("\nDone.\n\n")

  return(CompData)

} # End doSexRatio
