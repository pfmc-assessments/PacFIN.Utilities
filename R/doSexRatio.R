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
#' @param ratioU the ratio to apply to fish less than maxsizeU
#' @param maxsizeU the size below which the sex ratio is assumed ratioU.
#' @param GTsizeU the size above which the ratio is assumed to be 1.0 (big mamas).
#' @template savedir
#'
#' @details
#' Sex ratios may be assigned in one of four different ways:
#' 1. fish below \code{maxsizeU} will have the \code{ratioU} applied
#' 2. fish above \code{GTsizeU} are assumed to be female
#' 3. fish between \code{maxsizeU} and \code{GTsizeU} will have the observed
#' sex ratio of the known-sex fish applied
#' 4. when there are fewer than three known-sex fish in the length bin
#' observations from adjacent bins will be used
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
#' Andi Stephens, Kelli Johnson, and Chantel Wetzel
#' 
#' @seealso
#' \code{\link{getComps}}, \code{\link{writeComps}}
#'
#############################################################################

doSexRatio = function( CompData, ratioU, maxsizeU, GTsizeU, savedir ) {

  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!
  AGE_FLAG = FALSE
  if ( length(CompData$lengthcm) == 0 ) {
    index = which(names(CompData) == "Age")
    names(CompData)[index] = "lengthcm"
    AGE_FLAG = TRUE
  } # End if

  # Fix arithmetic
  CompList = c("male", "msamps", "mtows",
               "female", "fsamps", "ftows",
               "unsexed", "usamps", "ONLY_U_TOWS", 
               "alltows")
  tmp = CompData[,CompList]
  tmp[is.na(tmp)] = 0
  CompData[,CompList] = tmp
  
  if (!missing(ratioU) & missing(maxsizeU)) {
    stop("Error: The maxsizeU needs to be specified if you are using ratioU")    
  }


  calcRatio <- prop.table(x = as.matrix(CompData[, c("female", "male")]), margin = 1)[, 1]

  # Check to see if some years are only unsexed
  CompData$percent_unsexed <- CompData$usamps / 
   (CompData$usamps + CompData$fsamps + CompData$msamps)
  check <- aggregate(percent_unsexed~fishyr, CompData, function(x) sum(x == 1) / sum(x) )
  yrs <- check[,'percent_unsexed'] == 1 & is.finite(check[, 'percent_unsexed'])
  n   <- sum(yrs, na.rm = TRUE)
  if(n > 0 ) {
    message("Thera are ", n, " years (",
    unique(check[yrs,'fishyr']), ") with only unsexed fish. Applying sex ratio to these years may not be ideal.")
  }   

  # Deal with NaNs
  noRatio <- which(!is.finite(calcRatio))
  for(i in noRatio){
    # 1st: Try to fill in based on neighboring lengths within a year
    # Initially, I was also using fleet but appeared to lead to increased
    # variability in the sex ratio being calculated.
    nearLens <- c((CompData[i, 'lengthcm'] - 2):(CompData[i, 'lengthcm'] + 2))
    ind <- which(CompData[, 'fishyr'] == CompData[i, 'fishyr'] & 
                 CompData[ , 'lengthcm'] %in% nearLens )
    # Only use if the observations are 3 or greater
    # One will be the missing sex ratio, so a 3 or more would indicate more than
    # one near neighbor observation
    if (length(ind) >= 3 & sum(!is.finite(calcRatio[ind])) != length(ind)){
      calcRatio[i] <- mean(calcRatio[ind], na.rm = TRUE)     
    } else{
        # 2nd: Use sex ratio for the same length across all data
        ind <- which(CompData[, 'lengthcm'] == CompData[i,'lengthcm'])
        calcRatio[i] <- mean(calcRatio[ind], na.rm = T)
    }
  }

  # Now let's check small fish if user has pre-specified ratio to be applied
  if(!missing(maxsizeU)){
    ind <- which(CompData[, 'usamps'] > 0 & CompData[, 'lengthcm'] < maxsizeU) 
    if (length(ind) > 0){
      calcRatio[ind] <- ratioU
    }
  }

  if(!missing(GTsizeU)){
    ind <- which(CompData[, 'usamps'] > 0 & CompData[, 'lengthcm'] > GTsizeU)
    if (length(ind) > 0){
      calcRatio[ind] <- 1.0
    }
  }

  CompData$sexRatio <- calcRatio

  # Now let's apply the calcRatio to all the unsexed fish:
  for ( i in 1:nrow(CompData) ) {
    CompData$female[i] <- CompData$female[i] + CompData$unsexed[i] * calcRatio[i]
    CompData$fsamps[i] <- CompData$fsamps[i] + CompData$usamps[i] * calcRatio[i]
    CompData$ftows[i]  <- CompData$ftows[i]  + CompData$ONLY_U_TOWS[i] * calcRatio[i]
    CompData$male[i]   <- CompData$male[i]   + CompData$unsexed[i] * ( 1 - calcRatio[i] )
    CompData$msamps[i] <- CompData$msamps[i] + CompData$usamps[i] * ( 1 - calcRatio[i] )
    CompData$mtows[i]  <- CompData$mtows[i]  + CompData$ONLY_U_TOWS[i] * ( 1 - calcRatio[i] )
  } 

  if (!missing(savedir)) {
    find = which(CompData$usamps > 0 )
    png(file.path(savedir, "Applied_Sex_Ratio_to_Unsexed_Fish.png"))
      on.exit(dev.off(), add = TRUE)
      plot(CompData[find,'lengthcm'], CompData[find, 'sexRatio'],
        xlab = "Length (cm)", ylab = "Sex Ratio",
        main = paste0("Sex Ratio Applied to Unsexed Fish (N = ", sum(CompData$usamps), ")" )
      )
  }

  # If AGE comps, Bins are ages, not lengths.  Rename "age" to "lengthcm", then put
  # it back at the end!
  if (AGE_FLAG) {
    index = which(names(CompData) == "lengthcm")
    names(CompData)[index] = "Age"
  } # End if

  cat("\nDone.\n\n")

  return(CompData)

} # End doSexRatio
