##########################################################################
#'
#' Write out composition data formatted for Stock Synthesis.
#' 
#' @description
#' Write out composition data to a file, binning the data as specified.
#' 
#' \subsection{Workflow}{
#' PacFIN data should first be stratified using \code{\link{getComps}},
#' then sexes should be assigned as males and females by \code{\link{doSexRatio}}
#' before using \code{writeComps}.
#' 
#' \strong{Failure to use \code{\link{doSexRatio}} will result in all unsexed fish being discarded.}
#' }
#' 
#' @export
#'   
#' @param inComps A dataframe generated as described in Workflow, above.
#' 
#' @template fname
#'   
#' @param abins  Bins to use for ages.  Default is the data bins.
#' @param lbins  Bins to use for lengths.  Default is the data bins.
#' 
#' @param maxAge A numeric value specifying the maximum age of fish that
#'   should be included in the composition data, unless \code{dummybins = TRUE},
#'   then those fish will be included in a plus group which you can investigate
#'   later. Note that \code{maxAge} is only used if \code{abins = NULL}, otherwise
#'   fish are binned according to user specified bins irregardless of \code{maxAge}.
#'   
#' @param partition  Used by Stock Synthesis for length- or age-composition data
#' where 0 = retained + discarded, 1= discarded, and 2 = retained fish.
#' The default is to assume that these fish are retained only.
#' The default was changed in 2020 from a value of 0,
#' and code should be updated accordingly if you really want 0.
#' @param ageErr     Defaults to 1.
#' 
#' @param dummybins A logcial value specifying whether data outside of the
#'   lower and upper \code{abins} or \code{lbins} should be added to dummy bins,
#'   or be placed in the specified bins. Default is \code{TRUE}.  Dummy
#'   bins are useful for determining whether the current bin structure
#'   properly captures the data.
#'
#' @param sum1 A logical value specifying whether to rescale the compositions
#'   to sum to 1 (they will also be rounded to 4 decimal places)
#'
#' @param digits Integer indicating the number of decimal places to round
#'   value to (value is passed to \code{round()}). NULL will skip rounding.
#'
#' @param overwrite A logical value specifying whether to overwrite an existing
#'   file if the file associated with the input \code{fname} already exists.
#' 
#' @template verbose
#'   
#' @details
#' 
#'   The structure of the input dataframe determines whether
#'   \code{writeComps} produces age-, length-, or conditional-age-at-length-
#'   composition data.
#'   
#'   Four sets of composition data are written to a single file specified
#'   by \code{fname}.  These are:
#'   \itemize{
#'   \item{females followed by males}
#'   \item{male data only}
#'   \item{female data only}
#'   \item{males and females recombined as unsexed fish}
#'   }
#'   
#'   The output file is appended to, rather than overwritten, so you may
#'   want to specify a new filename each time you generate a different
#'   stratification or bin structure, e.g., "out.1.csv", "out.2.csv".
#'   
#'   Composition data are raw weights rather than proportions. Stock Synthesis
#'   internally converts these to proportions.  The raw weights should be
#'   examined for anomalies.
#'   
#'   To create proportions use \code{prop.table} on the columns
#'   containing composition data.
#' 
#'   The columns in the output preceeding 'lengthcm' or 'age' are those that
#'   were used in stratifying the data.
#'   
#' \subsection{Reality Checks}{
#' \itemize{
#' \item{Set \code{verbose = TRUE} to follow progress}
#' \item{Use \code{dummybins} to evaluate bin structure}
#' \item{Examine the raw-weight output for anomalies}
#' }}
#' @return Appends data to the file given in \code{fname}.
#'  
#'   Conditionally returns a dataframe if the \code{out} argument
#'   specifies the type of composition to return.
#'   
#' @author Andi Stephens, Chantel Wetzel, Kelli Johnson, Ian Taylor
#' @seealso \code{\link{getComps}}, \code{\link{doSexRatio}}
#'
##############################################################################
writeComps = function(inComps, fname = NULL, abins = NULL, lbins = NULL,
                      maxAge = Inf, partition = 2, ageErr = 0, 
                      dummybins = FALSE, sum1 = FALSE, digits = 4,
                      overwrite = TRUE, verbose = FALSE) {

  # To stop warning of no visible binding b/c assign is used
  mComps <- NULL
  fComps <- NULL
  uComps <- NULL

  lbins_in <- lbins 
  abins_in <- abins 

  # Which comps are we doing?
  Names = names(inComps)
  AGE = which(Names == "Age")
  LEN = which(Names == "lengthcm")

  if(is.null(fname)){
    if(length(LEN) > 0) { fname = "PacFIN_lengths.out"}
    if(length(AGE) > 0) { fname = "PacFIN_ages.out" }
    if(length(AGE) > 0 & length(LEN) > 0) { fname = "PacFIN_CAAL.out"}
  }

  if (verbose){
    cat(paste("Writing comps to file", fname, "\n"))
    #cat(paste("\nNote that if you didn't run doSexRatio,",
    #  "all unsexed fish disappear at this point.\n\n"))
    utils::flush.console()
  }

  dir.create(
    path = dirname(normalizePath(fname, mustWork = FALSE)),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # check for existence of the file before writing anything
  if(file.exists(fname)){
    if(overwrite){
      warning("The file ", fname,
              "\n  exists, and overwrite = TRUE, ",
              "so deleting the file before writing new tables.")
      file.remove(fname)
    }else{
      stop("The file ", fname, "\n  exists and overwrite = FALSE.")
    }
    utils::flush.console()
  }

  # Adding columns in case a sex is not represented in inComps
  if(length(inComps$male) == 0) {
    inComps$male <- inComps$msamps <- inComps$mtows <- 0
  }
  if(length(inComps$female) == 0) {
    inComps$female <- inComps$fsamps <- inComps$ftows <- 0
  }
  if(length(inComps$both) == 0) {
    inComps$both <- inComps$both <- inComps$both <- 0
  }
  if(length(inComps$unsexed) == 0){
    inComps$unsexed <- inComps$usamps <- inComps$utows <- 0
  }

  # Fix length bins
  if ( !is.null(inComps$lengthcm) ) {
    if ( is.null(lbins) ) {
      if (verbose) {
        cat("\nNo length bins provided, using data as-is\n\n")
      }
      lbins = sort(unique(inComps$lengthcm))
    } # End if for lbins

    # Re-code actual lengths to be lbins
    # Note that the last bin is a dummy bin,
    # created because of how findInterval works. It is stripped later.
    if (dummybins) {
      # KFJ(2015-06-15): Only add zero to the left tail if the user
      # does not want this data included in the compositions and
      # wants to investigate dummy bins, else just include it in the
      # smallest group because it is a pain to have to add back later.
      if (min(lbins) > 0) {
        lbins <- c(0, lbins)
        LbinLo <- lbins
      }
      # KFJ(2015-06-15): Add a bin at the end of the userspecified
      # bins that is of the same bin width to capture the plus group
      # if people want to look at this separately and add back later
      # The Inf bin will work better than 999 b/c you theoretically
      # can have a fish with length larger than 999, but not Inf.
      lbins <- c(lbins, max(lbins) + diff(utils::tail(lbins, 2)), Inf)
    } else {
      lbins <- c(lbins, Inf)
      LbinLo <- c(0, lbins[-1])
    }

    LbinHi <- c(lbins[-1] - 1, Inf)
    inComps$lbin <- findInterval(inComps$lengthcm, lbins, all.inside = TRUE)
  } # End if for lbins

  # Fix age bins
  if (!is.null(inComps$Age)) {
    if ( is.null(abins) ) {
      if (verbose){
        cat("\nNo age bins provided, using data as-is\n\n")
      }
      abins = sort(unique(inComps$Age))
      abins = abins[abins < maxAge]
    } # End if for abins

    # Re-code actual ages to be abins
    if (dummybins) {      
      if (min(abins) > 0) {
        abins = c(0, abins)
      }      
      abins <- c(abins, max(abins) + diff(utils::tail(abins, 2)), Inf)
    } else {      
      abins <- c(abins, Inf)
    } # End if-else dummybins

    # add extra, dummy bin because all.inside=T
    inComps$abin = findInterval(inComps$Age, abins, all.inside = TRUE)
  } # End if inComps$ages

  AAL = FALSE
  if ( length(AGE) > 0 ) {
    target <- "abin"
    STRAT  <- AGE - 1
    KeyNames <- c(Names[1:STRAT])
    inComps$key <- apply(inComps[,KeyNames, drop = FALSE], 1, paste, collapse = " ")

    # matrix will be Ages, Ntows, Nsamps.
    # it gets re-ordered later.
    NCOLS <- 2 + length(abins)
    OutNames <- c(paste("A", abins, sep=""), "Ntows","Nsamps")

    if ( length(LEN) > 0 ) {
      AAL <- TRUE
      STRAT <- AGE - 2
      KeyNames <- c(Names[1:STRAT], "lbin")
      inComps$key <- apply(inComps[,KeyNames, drop = FALSE], 1, paste, collapse = " ")

      # matrix will be Ages, LbinLo, LbinHi, Ntows, Nsamps.
      # it gets re-ordered later.
      NCOLS <-  4 + length(abins)
      OutNames <- c(paste("A", abins, sep = ""), "lbin","Ntows","Nsamps")
    } # End if
  } else {
    target <- "lbin"
    STRAT <- LEN - 1
    KeyNames <- c(Names[1:STRAT])
    inComps$key <- apply(inComps[,KeyNames, drop = FALSE], 1, paste, collapse = " ")

    # matrix will have Lbins, Ntows, Nsamps
    # it gets re-ordered later.
    NCOLS <- 2 + length(lbins)
    OutNames <- c(paste0("L",lbins), "Ntows","Nsamps")
  } # End if-else

  # Rename columns to be used below
  if(!AAL){
    names(inComps)[which(names(inComps) == "both")] <- "b"
    names(inComps)[which(names(inComps) == "female")] <- "f"
    names(inComps)[which(names(inComps) == "male")]   <- "m"
    names(inComps)[which(names(inComps) == "unsexed")]<- "u"
  } else {
    # Overwrite the expansion value to match sample sizes for AAL
    # This should really be done in the getComps function
    inComps$f <- inComps$fsamps
    inComps$m <- inComps$msamps
    inComps$u <- inComps$usamps
  } 

  # We'll work key by key
  uKeys <- inComps$key[!duplicated(inComps$key)]

  # Save the matching stratification columns
  uStrat <- inComps[!duplicated(inComps$key), 1:STRAT]

  if (verbose) {
    cat(length(uKeys), "unique keys for", nrow(inComps), "records\n\n")
    #head(inComps)
    cat("\n\n")
    utils::flush.console()
  }

  # For each sex in turn
  for ( g in c("m","f","u","b")) {
    myname <- g
    if (verbose) {
      cat(paste("Assembling, sex is:", myname, "\n"))
      utils::flush.console()
    }
    tows  <- which(names(inComps) == paste(g, "tows", sep = ""))
    samps <- which(names(inComps) == paste(g, "samps", sep = ""))

    # Create output matrix
    output <- data.frame(matrix(nrow = length(uKeys), ncol = NCOLS, 0))
    names(output) <- OutNames

    for ( k in 1:length(uKeys) ) {
      # Get the matching records
      slice <- inComps[inComps$key == uKeys[k], ]

      if ( AAL ) {
        output$lbin[k] <- slice$lbin[1]
      } # End if
      output$Nsamps[k] <- sum(slice[,samps], na.rm = TRUE)

      # Use max here to take care of spurious NA problem that arises in getComps
      # for lengths where there are unsexed fish and no sexed fish.
      output$Ntows[k] <- max(slice[,tows], na.rm = TRUE)

      for ( s in 1:length(slice[,target]) ) {
          index <- slice[s, target]
          output[k, index] <- slice[s, g] + output[k, index]
      } # End for s

    } # End for k

    # Save and identify
    output[is.na(output)] <- 0

    if ( AAL ) {
      output$LbinLo <- LbinLo[output$lbin]
      # The low and high lbin on AAL typically are the same and match
      # the lower lbin
      output$LbinHi <- LbinLo[output$lbin] #LbinHi[output$lbin]
    } # End if

    # assign to a name like 'mComps' or 'fComps'
    assign(paste(g, "Comps", sep = ""), output)
  } # End for g


  # Now assemble everything and write to a file
  # Note that we are stripping the last, dummy bin.
  NCOLS <- ifelse(AAL, NCOLS - 5, NCOLS - 3)

  # Creating a matrix of a specific size (mComps)
  # which is then being erased (blanks[,] <- 0)
  blanks <- mComps[1:NCOLS]
  blanks[,] <- 0

  if (!"fishyr" %in% colnames(uStrat)) stop("fishyr should be a column")
  if (!"fleet"  %in% colnames(uStrat)) stop("fleet should be a column")
  uStrat <- data.frame(uStrat[, "fishyr"], 
                       month = 1,
                       uStrat[, 'fleet'])
  colnames(uStrat) <-  c("year", "month", "fleet")

  # Fill the rest of the values
  uStrat$sex <- NA
  uStrat$partition <- partition

  if ( length(AGE) > 0 ) {
    uStrat$ageErr <- ageErr
    uStrat$LbinLo <- -1
    uStrat$LbinHi <- -1
  }

  if ( AAL ) {
    # Note that until empty rows are removed, the LbinLo and LbinHi columns
    # are the same in each dataset
    uStrat$ageErr <- ageErr
    uStrat$LbinLo <- fComps$LbinLo
    uStrat$LbinHi <- fComps$LbinHi
  }

  Ninput_b <- round(ifelse( 
                      bComps$Nsamps / bComps$Ntows < 44,
                      bComps$Ntows + 0.138 * bComps$Nsamps,
                      7.06 * bComps$Ntows), 0 )
  Ninput_b[is.na(Ninput_b)] <- 0
  Ninput_f <- round(ifelse( 
                      fComps$Nsamps / fComps$Ntows < 44,
                      fComps$Ntows + 0.138 * fComps$Nsamps,
                      7.06 * fComps$Ntows), 0 )
  Ninput_f[is.na(Ninput_f)] <- 0
  Ninput_m <- round(ifelse( 
                      mComps$Nsamps / mComps$Ntows < 44,
                      mComps$Ntows + 0.138 * mComps$Nsamps,
                      7.06 * mComps$Ntows), 0 )
  Ninput_m[is.na(Ninput_m)] = 0
  Ninput_u <- round(ifelse( 
                      uComps$Nsamps / uComps$Ntows < 44,
                      uComps$Ntows + 0.138 * uComps$Nsamps,
                      7.06 * uComps$Ntows), 0 )
  Ninput_u[is.na(Ninput_u)] <- 0

  if(!AAL){
    if(is.null(lbins_in)) {
      bins <- abins_in
    } else {
      bins <- lbins_in
    }
    FthenM <- cbind(uStrat, round(bComps$Ntows, 0), round(bComps$Nsamps, 0), Ninput_b,
                    fComps[,1:NCOLS], mComps[,1:NCOLS])
    index <- grep("Ninput", names(FthenM))
    names(FthenM)[(index + 1):ncol(FthenM)] <- c(paste0('F', bins), paste0("M", bins))
    
    Fout  <- cbind(uStrat, round(fComps$Ntows, 0), round(fComps$Nsamps, 0), Ninput_f,
                    fComps[,1:NCOLS], fComps[,1:NCOLS])
    index <- grep("Ninput", names(Fout))
    colnames(Fout)[(index + 1):ncol(Fout)] <- c(paste0('F', bins), paste0("F.", bins))
    
    Mout  <- cbind(uStrat, round(mComps$Ntows, 0), round(mComps$Nsamps, 0), Ninput_m,
                    mComps[,1:NCOLS], mComps[,1:NCOLS])
    index <- grep("Ninput", names(Mout))
    colnames(Mout)[(index + 1):ncol(Mout)] <- c(paste0('M', bins), paste0("M.", bins))
    
    Uout  <- cbind(uStrat, round(uComps$Ntows, 0), round(uComps$Nsamps, 0), Ninput_u,
                     uComps[,1:NCOLS], uComps[,1:NCOLS])
    index <- grep("Ninput", names(Uout))
    colnames(Uout)[(index + 1):ncol(Uout)] <- c(paste0('U', bins), paste0("U.", bins))
  } else {
    # AAL
    Fout <- cbind(uStrat, fComps$Nsamps, fComps[,1:NCOLS], fComps[,1:NCOLS])
    Mout <- cbind(uStrat, mComps$Nsamps, mComps[,1:NCOLS], mComps[,1:NCOLS])
    Uout <- cbind(uStrat, uComps$Nsamps, uComps[,1:NCOLS], uComps[,1:NCOLS])
    FthenM <- NULL
  }

  # Make it pretty
  if(!AAL) { 
    index <- grep("Ntows", names(Fout))
    names(Mout)[index]   <- "Ntows"
    names(Fout)[index]   <- "Ntows"
    names(Uout)[index]   <- "Ntows"
    names(FthenM)[index] <- "Ntows" 
  }

  index <- grep("Nsamp", names(Fout))
  names(Mout)[index]   <- "Nsamps"
  names(Fout)[index]   <- "Nsamps"
  names(Uout)[index]   <- "Nsamps"
  if(!AAL) { names(FthenM)[index] <- "Nsamps" }

  if(!AAL){
    index <- grep("Ninput", names(Fout))
    names(Mout)[index]   <- "InputN"
    names(Fout)[index]   <- "InputN"
    names(Uout)[index]   <- "InputN"
    names(FthenM)[index] <- "InputN"
  }

  # Remove empty rows
  FthenM <- FthenM[FthenM$Nsamps > 0, ]
  Fout <- Fout[Fout$Nsamps > 0, ]
  Mout <- Mout[Mout$Nsamps > 0, ]
  Uout <- Uout[Uout$Nsamps > 0, ]

  if(dim(Uout)[1] != 0)   { Uout$sex <- 0}
  if(dim(Fout)[1] != 0)   { Fout$sex <- 1}
  if(dim(Mout)[1] != 0)   { Mout$sex <- 2}
  if(!AAL) { if(dim(FthenM)[1] != 0) { FthenM$sex <- 3 } }

  # function to rescale comps to sum to 1
  # IGT(2019-04-25): I tried using an apply function but kept messing up,
  # so fell back on a simple loop over the rows
  rescale.comps <- function(out){
    value.names <- grep("^[alfmuALFMU][0-9]+", colnames(out), value = TRUE)
    for(irow in 1:nrow(out)){
      out[irow, names(out) %in% value.names] <-
        out[irow, names(out) %in% value.names] /
          sum(out[irow, names(out) %in% value.names])
    }
    # Code to apply the rescaled comps to the matrices
    # for Mout, Fout, or Uout composition data. The adj
    # value is based on the number of informational columns
    # prior (year, fleet, partition) to the composition data. 
    if ("ageErr" %in% colnames(out)){
      adj <- 11
    } else {
      adj <- 8
    }
    # Only enter this if statement for the Mout, Fout, or
    # Uout composition data. This allows the rounding to be
    # applied to that second copy print of the composition data.
    if (length(value.names) < (dim(out)[2] - adj)){
      find <- which(names(out) == value.names[1])
      ind <- (find + length(value.names)):dim(out)[2]        
      out[, ind] <- out[, names(out) %in% value.names]
    }
    return(out)
  }

  # function to round comps
  round.comps <- function(out, digits){
    value.names <- grep("^[alfmuALFMU][0-9]+", colnames(out), value = TRUE)
    out[, names(out) %in% value.names] <-
      round(out[, names(out) %in% value.names], digits = digits)

    if ("ageErr" %in% colnames(out)){
      adj <- 11
    } else {
      adj <- 8
    }

    if (length(value.names) < (dim(out)[2] - adj)){
      find <- which(names(out) == value.names[1])
      ind <- (length(value.names) + find):dim(out)[2]
      out[, ind] <- 
        round(out[, names(out) %in% value.names], digits = digits)
    }
    return(out)
  }
  
  # optionally rescale to sum to 1
  # this needs to happen after combining FthenM rather than to
  # the sex-specific parts
  if (sum1 & !AAL) {
    if (verbose) {
      message("rescaling comps to sum to 1")
    }
    if (dim(Uout)[1] != 0) { Uout <- rescale.comps(Uout) }
    if (dim(Fout)[1] != 0) { Fout <- rescale.comps(Fout) }
    if (dim(Mout)[1] != 0) { Mout <- rescale.comps(Mout) }
    if (!AAL) { 
      if (dim(FthenM)[1] != 0) { 
        FthenM <- rescale.comps(FthenM) 
      } 
    }
  }

  # optionally round off to chosen value
  if (!missing(digits)) {
    if (verbose){
      message("rounding values to ", digits, " digits")
    }
    if (dim(Uout)[1] != 0) {
      Uout <- round.comps(Uout, digits = digits)
    }
    if (dim(Fout)[1] != 0) {
      Fout <- round.comps(Fout, digits = digits)
    }
    if (dim(Mout)[1] != 0) {
      Mout <- round.comps(Mout, digits = digits)
    }
    if (!AAL) {
      if (dim(FthenM)[1] != 0) {
        FthenM <- round.comps(FthenM, digits = digits)
      }
    }
  }

  # Print the whole shebang out to a file.
  ## # Turn off warnings about "appending column names to file"
  oldwarn = options("warn")
  options("warn" = -1)

  if (verbose) {
    cat("Writing FthenM, dimensions:", dim(FthenM), "\n")
  }
  IDstring = paste("\n\n", "Females then males")
  cat(file = fname, IDstring, "\n", append = FALSE)  
  utils::write.table(file = fname, FthenM, sep = ",", col.names = TRUE,
    row.names = FALSE, append = TRUE)

  if (verbose) {
    cat("Writing F only, dimensions:", dim(Fout), "\n")
  }
  IDstring = paste("\n\n", "Females only")
  cat(file = fname, IDstring, "\n", append = TRUE)  
  utils::write.table(file = fname, Fout, sep = ",", col.names = TRUE,
    row.names = FALSE, append = TRUE)
  
  if (verbose) {
    cat("Writing M only, dimensions:", dim(Mout), "\n")
  }  
  IDstring = paste("\n\n",  "Males only")
  cat(file = fname, IDstring, "\n", append = TRUE)
  utils::write.table(file = fname, Mout, sep =",", col.names = TRUE,
    row.names = FALSE, append = TRUE)

  if (verbose) {
    cat("Writing U only, dimensions:", dim(Uout), " \n")
  } 

  if("sexRatio" %in% colnames(inComps)){
    u_message <- paste("\n\n", 
      "Sex ratio was applied and unsexed are included above: only reported as a diagnostic") 
  } else {
    u_message <- paste("\n\n", "Usexed only")
  }

  IDstring = u_message
  cat(file = fname, IDstring, "\n", append = TRUE)
  utils::write.table(file = fname, Uout, sep = ",", col.names = TRUE,
    row.names = FALSE, append = TRUE)
 
  # Reset warnings
  #options("warn" = oldwarn[[1]])
  #invisible(eval(parse(text = returns)))

  # return tables
  invisible(list(FthenM = FthenM,
                 Fout = Fout,
                 Mout = Mout,
                 Uout = Uout))
} # End function writeComps
