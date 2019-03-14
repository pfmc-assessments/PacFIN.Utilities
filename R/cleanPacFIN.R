######################################################################
#
#' Filter PacFIN samples.
#'
#' @description
#' \code{cleanPacFIN} filters out unsuitable samples from data, and converts
#' fish lengths to cm. The original fields in the returned data are left untouched,
#' with the exception of SEX, which is modified so that unidentified fish are labeled
#' "U".
#' 
#' \subsection{\code{\link{Workflow}}}{
#' If there are CalCOM samples to be integrated with PacFIN data, run \code{combineCalCOM}
#' first, otherwise run to \code{cleanPacFIN} as the first function in the workflow.
#' }
#' 
#' @export
#'
#' @param Pdata a PacFIN dataset
#' @param only_USINPFC a logical value. FALSE by default.
#' @param keep_INPFC a set of INPFC areas. NULL by default.
#' @param remove_INPFC a set of INPFC areas.  NULL by default.
#' @param badRecords a set of sample identifiers. NULL by default.
#' @param keep_sample_type a set of sample types to retain.  Default = c("", "M")
#' @param keep_sample_method a set of sample methods to retain.  Default = "R"
#' @param keep_missing_lengths a logical value. FALSE by default.
#' @param keep_CA default TRUE.  CA data often have no sample type or method, or INPFC area.
#' @param CLEAN a logical value.  Default is TRUE.  If FALSE, return the original data unchanged,
#' but report what would have been removed.
#' 
#' @return The input data filtered for desired areas and record types
#' specified, with added columns
#' 
#' \tabular{ll}{ 
#'   fishyr \tab initialized from SAMPLE_YEAR\cr
#'   fleet \tab initialized to 1\cr
#'   fishery \tab initialized to 1\cr
#'   season \tab initialized to 1.  Change using \code{\link{getSeason}}\cr
#'   state \tab initialized from SOURCE_AGID.  Change using \code{\link{getState}}\cr
#'   lengthcm \tab floored cm from FORK_LENGTH when available, otherwise FISH_LENGTH\cr
#'   geargroup \tab the gear group associated with each GRID, from http://pacfin.psmfs.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
#' }
#' 
#' 
#' @details
#' 
#' \subsection{\strong{INPFC Area specification}}{
#' 
#' The US INPFC areas are 
#'    c("VUS","CL","VN","COL","NC","SC","EU","CALCOM","CP","EK","MT","PS ")
#'    
#' "CalCOM" is included because the combineCalCOM function
#' sets it, since CalCOM doesn't seem to record INPFC areas.
#' 
#' 
#' If \code{only_USINPFC} is TRUE, then only samples from the US INPFC areas will be retained.
#' 
#' If a set of INPFC areas are specified in \code{keep_INPFC}, then only samples from 
#' those areas will be retained.
#' 
#' If \code{remove_INPFC} specifies a set of INPFC areas, samples from those areas
#' will be discarded.
#' }
#' 
#' \subsection{\strong{Sample types and methods}}{
#' 
#' SAMPLE_TYPEs may be (M=Market, R=Research, S=Special request, C=Commercial on-board).
#' Only samples of type M are generally used.
#' 
#' SAMPLE_METHODs may be (R=Random, S=Stratified, N=Systematic, P=Purposive, X=Special).
#' Only samples collected in random sampling are generally used.
#' }
#' 
#' \subsection{\strong{Furthermore}}{
#' 
#' The values created as new columns are for use by other functions in this package.
#' In particular, \code{fishyr} and \code{season} are useful if there are multiple 
#' seasons (e.g., winter and summer, as in the petrale sole assessment), and the 
#' year is adjusted so that "winter" occurs in one year, rather than across two.
#' 
#' The \code{fleet}, \code{fishery}, and \code{state} columns are meant for use in
#' stratifying the data according to the particulars of an assessment.
#' 
#' The \code{sink} command can be used to save the filtering report to a 
#' file, in addition to printing it to the console.
#' }
#' 
#' @seealso \code{\link{cleanAges}}, \code{\link{getState}}, \code{\link{getSeason}}, 
#' \code{\link{sink}}
#'
#' @author Andi Stephens
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
                        keep_CA = TRUE,
                        CLEAN=TRUE) {

  cat( "\nCleaning data\n\n" )

  if (!CLEAN) {

    cat("\nGenerating data report only.  No data will be removed.\n")

    Original_data = Pdata
  }
  
  
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
  
  Pdata = getState(Pdata)
  cat("Pdata$state is initialized to Pdata$SOURCE_AGID\n")
  
  Pdata$fishyr = Pdata$SAMPLE_YEAR
  cat("Pdata$fishyr is initialized to Pdata$SAMPLE_YEAR\n")
  
  Pdata = getGearGroup(Pdata)
  
  if (keep_CA) {
    
    CAdata = Pdata[Pdata$state == "CA",]
    Pdata = Pdata[!Pdata$state == "CA",]
    
    CAdata$SAMPLE_TYPE[is.na(CAdata$SAMPLE_TYPE)] = "M"
    CAdata$SAMPLE_METHOD[is.na(CAdata$SAMPLE_TYPE)] = "R"
    CAdata$INPFC_AREA[is.na(CAdata$INPFC_AREA)] = "CalCOM"
  
    Pdata = rbind(Pdata, CAdata)
    
  } # End keep_CA

  # Define legal areas.  "CalCOM" is included because the combineCalCOM function
  # sets it -- CalCOM doesn't seem to record INPFC areas.


  USinpfc = c("VUS","CL","VN","COL","NC","SC","EU","CalCOM","CP","EK","MT","PS ")

  # Fix Lengths.  Use FISH_LENGTH if there is no FORK_LENGTH.

  Pdata$FORK_LENGTH[is.na(Pdata$FORK_LENGTH)] = -1
  Pdata$length = ifelse(Pdata$FISH_LENGTH > -1, Pdata$FISH_LENGTH, Pdata$FORK_LENGTH)

  # Convert mm to cm

  Pdata$lengthcm = floor(Pdata$length / 10)

  # We don't want no stinkin' NAs!

  Pdata$SEX[is.na(Pdata$SEX)] = "U"
  Pdata$SEX[Pdata$SEX == 0 ] = "U"

  # Fix Ages (originally in cleanAges)
  # MH is checking with JW to see if there is a AGE_METHOD per age reader
  Pdata$age <- ifelse(!is.na(Pdata$FISH_AGE_YEARS_FINAL), 
    Pdata$FISH_AGE_YEARS_FINAL, Pdata$age1)
  Pdata$age <- ifelse(!is.na(Pdata$age), Pdata$age, Pdata$age2)
  Pdata$age <- ifelse(!is.na(Pdata$age), Pdata$age, Pdata$age3)
  Pdata$age[is.na(Pdata$age)] <- -1
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

  if (CLEAN) {

    return(Pdata)

  } else {

    cat("\n\nReturning original data because CLEAN=FALSE\n\n")

    return(Original_data)

 } # End if-else

} # End cleanPacFIN
