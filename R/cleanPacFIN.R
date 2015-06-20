
##############################################################################
#
#  cleanPacFIN filters out unusable data, and fixes lengths.
#
#  Fields to use post-cleaning are:
#
#  length
#
#  The original fields (in the retained data) are left untouched for
#  diagnostics.
#
##############################################################################

cleanPacFIN = function( Pdata,
                        only_USINPFC = FALSE,
            keep_INPFC = NULL,
            remove_INPFC = NULL,
                        badRecords = NULL,
                        keep_sample_type = c("", "M"),
                        keep_sample_method = "R",
            keep_missing_lengths = FALSE,
                        CLEAN=TRUE) {

  cat( "\nCleaning data\n\n" )

  if (!CLEAN) {

    cat("\nGenerating data report only.  No data will be removed.\n")

    Original_data = Pdata
  }

  # Define legal areas

  # This is a legacy comment from Owen's POP code.  Need to investigate.
  # NOTE only a few with "" from 2005 and 2010. NEARLY 100,000 are from CHR
  # and VCN (through 1978) - a large proportion of early data.

  USinpfc = c("VUS","CL","VN","COL","NC","SC","EU","CALCOM","CP","EK","MT","PS ")

  # Fix Lengths.  Use FISH_LENGTH if there is no FORK_LENGTH.

  Pdata$FORK_LENGTH[is.na(Pdata$FORK_LENGTH)] = -1
  Pdata$length = ifelse(Pdata$FISH_LENGTH > -1, Pdata$FISH_LENGTH, Pdata$FORK_LENGTH)

  # Convert mm to cm

  Pdata$lengthcm = floor(Pdata$length / 10)

  # Fix EXP_WT:  Used in expansions.  Zero works arithmetically, NA does not.

  Pdata$exp_wt = Pdata$EXP_WT
  Pdata$exp_wt[is.na(Pdata$EXP_WT)] = 0

  # We don't want no stinkin' NAs!

  Pdata$SEX[is.na(Pdata$SEX)] = "U"
  Pdata$SEX[Pdata$SEX == 0 ] = "U"

  # Flag records without a SAMPLE_NO

  Pdata$sample = Pdata$SAMPLE_NO

  # KFJ: use more values than just NA, also only do if TRUE
  # Andi:  thanks!

  flags <- c("NA", "Nan", "")

  if (any(Pdata$sample %in% flags)) {

    Pdata$sample[Pdata$sample %in% flags] <- "-1"

  } # End if

  # Remove records
  Rec_summary = rep(0,7)

  Rec_summary[1] = nrow(Pdata)

  if (only_USINPFC == TRUE) { Pdata = Pdata[Pdata$INPFC_AREA %in% USinpfc,] }

  if (! is.null(keep_INPFC) ) { Pdata = Pdata[Pdata$INPFC_AREA %in% keep_INPFC,] }
  if (! is.null(remove_INPFC) ) { Pdata = Pdata[!Pdata$INPFC_AREA %in% remove_INPFC,] }

  Rec_summary[2] = nrow(Pdata)

  Pdata = Pdata[!Pdata$sample %in% badRecords,]

  Rec_summary[3] = nrow(Pdata)

  if (! is.null(keep_sample_type)) { Pdata = Pdata[Pdata$SAMPLE_TYPE %in% keep_sample_type,] }

  Rec_summary[4] = nrow(Pdata)

  if (! is.null(keep_sample_method) ) { Pdata = Pdata[Pdata$SAMPLE_METHOD %in% keep_sample_method,] }

  Rec_summary[5] = nrow(Pdata)

  Pdata = Pdata[Pdata$SAMPLE_NO != -1,]

  Rec_summary[6] = nrow(Pdata)

  if (!keep_missing_lengths) { Pdata = Pdata[!is.na(Pdata$length),] }

  Rec_summary[7] = nrow(Pdata)

  # Report removals

  cat("\nRemoval Report\n\n")
  cat("Records in input:                 ", Rec_summary[1], "\n")
  cat("Records not in INPFC_AREA:        ", Rec_summary[1] - Rec_summary[2], "\n")
  cat("Records in badRecords list:       ", Rec_summary[2] - Rec_summary[3], "\n")
  cat("Records with bad SAMPLE_TYPE      ", Rec_summary[3] - Rec_summary[4], "\n")
  cat("Records with bad SAMPLE_METHOD    ", Rec_summary[4] - Rec_summary[5], "\n")
  cat("Records with no SAMPLE_NO         ", Rec_summary[5] - Rec_summary[6], "\n")
  cat("Records with no usable length     ", Rec_summary[6] - Rec_summary[7], "\n")
  cat("Records remaining:                ", nrow(Pdata), "\n\n")


  # Define fishyr, fleet, fishery and season  -- some assessments manipulate these.

  cat("These values have been initialized for use when comps are generated.\n")
  cat("Use Stratify and getSeason to reset them to appropriate values.\n\n")

  # KFJ: only create columns if they do not exist or if they are not numeric

  for ( i in c("fleet","fishery","season") ) {

    if (!i %in% colnames(Pdata)) {

      tmpcol = ncol(Pdata) + 1
      tmp = rep(1, nrow(Pdata))

      Pdata = cbind(Pdata, tmp)
      names(Pdata)[tmpcol] = i

      cat("Pdata$",i," = 1\n")

    } # End if

  } # End for

  Pdata$fishyr = Pdata$SAMPLE_YEAR
  cat("Pdata$fishyr is initialized to Pdata$SAMPLE_YEAR\n")

  if (CLEAN) {

    return(Pdata)

  } else {

    cat("\n\nReturning original data because CLEAN=FALSE\n\n")

    return(Original_data)

 } # End if-else

} # End cleanPacFIN
