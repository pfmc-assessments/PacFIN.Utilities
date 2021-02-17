#' Clean raw PacFIN biological data
#'
#' Clean raw data from PacFIN to remove unsuitable samples and
#' convert units to those appropriate for the rest of the code.
#'
#' @export
#'
#' @template Pdata
#' @param keep_INPFC *Deprecated*. Areas are now defined using different methods.
#' @param keep_gears A vector of character values specifying which gear types you want
#' to label as unique fleets. Order the vector the same way you want the fleets numbered.
#' If the argument is missing, which is the default, then all found gear groups
#' are maintained and order alphabetically. For more details see documentation for
#' \code{?\link{getGearGroup}} that lists a web link for where you can find the
#' available gear groupings and how they link to \code{"GRID"} within your data.
#' @param keep_sample_type A vector of character values specifying the types of
#' samples you want to keep. The default is to keep \code{c("", "M")}. Available
#' types include market (M), research (R), special request (S), and
#' commercial on-board (C).
#' @param keep_sample_method A vector of character values specifying the types of
#' sampling methods you want to keep. The default is to keep \code{"R"}, which
#' refers to samples that were sampled randomly. Available types include
#' random (R), stratified (S), systematic (N), purposive (P), and special (X).
#' @param keep_length_type A vector of character values specifying the types of
#' length samples to keep. There is no default value, though users will typically
#' want to keep \code{c("", "F", "A")}, but should also think about using
#' \code{c("", "F", "A", NA)}.
#' @param keep_age_method A vector of ageing methods to retain in the data. All fish
#' aged with methods other than those listed will no longer be considered aged.
#' A value of \code{NULL}, the default, will keep all ageing methods. However,
#' a vector of \code{c("B", "S", "", NA, 1, 2)} will keep all unaged fish and those
#' that were aged with break and burn and surface reads.
#' @param keep_missing_lengths *Deprecated*. Just subset them using
#' `is.na(Pdata[, 'length']) after running `cleanPacFIN` if you want to remove
#' lengths, though there is no need because the package accommodates keeping them in.
#' @param keep_states A vector of states that you want to keep, where each state
#' is defined using a two-letter abbreviation, e.g., `WA`. The default is to keep
#' data from all three states, even though California data often do not
#' have information on sample type or sample method. The default,
#' `keep_states = c("WA", "OR", "CA")`, is to keep the data.
#' Add `'UNK'` to the vector if you want to keep data not assigned to a state.
#' @param CLEAN A logical value used when you want to remove data from the input
#' data set. The default is \code{TRUE}. Where the opposite returns the original
#' data with additional columns and reports on what would have been removed.
#' @template spp
#' @template verbose
#' @template savedir
#'
#' @return The input data filtered for desired areas and record types
#' specified, with added columns
#'
#' \tabular{ll}{
#'   year \tab initialized from SAMPLE_YEAR\cr
#'   fleet \tab initialized to 1\cr
#'   fishery \tab initialized to 1\cr
#'   season \tab initialized to 1.  Change using \code{\link{getSeason}}\cr
#'   state \tab initialized from SOURCE_AGID.  Change using \code{\link{getState}}\cr
#'   lengthcm \tab floored cm from FORK_LENGTH when available, otherwise FISH_LENGTH\cr
#'   geargroup \tab the gear group associated with each GRID, from http://pacfin.psmfs.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
#' }
#'
#' @details
#' The original fields in the returned data are left untouched,
#' with the exception of
#' * `SEX`: modified using [getSex] and upon return will only include
#' character values such that fish with an unidentified sex are now `"U"`.
#' * age: the best ages to use going forward rather than just the first age read.
#'
#' \subsection{Workflow}{
#' If there are CalCOM samples to be integrated with PacFIN data, run \code{combineCalCOM}
#' first, otherwise run to \code{cleanPacFIN} as the first function in the workflow.
#' }
#'
#' \subsection{Furthermore}{
#' The values created as new columns are for use by other functions in this package.
#' In particular, \code{fishyr} and \code{season} are useful if there are multiple
#' seasons (e.g., winter and summer, as in the petrale sole assessment), and the
#' year is adjusted so that "winter" occurs in one year, rather than across two.
#'
#' The \code{fleet}, \code{fishery}, and \code{state} columns are meant for use in
#' stratifying the data according to the particulars of an assessment.
#' }
#'
#' @seealso \code{\link{getState}}, \code{\link{getSeason}}
#'
#' @author Andi Stephens
#' @examples
#' data(XMPL.BDS)
#' Pdata <- cleanPacFIN(XMPL.BDS, keep_length_type = unique(XMPL.BDS[, "FISH_LENGTH_TYPE"]))
#' NROW(XMPL.BDS) - NROW(Pdata)
#' # This time don't clean it
#' Pdata <- cleanPacFIN(XMPL.BDS, keep_length_type = unique(XMPL.BDS[, "FISH_LENGTH_TYPE"]),
#'   CLEAN = FALSE)
#' NROW(XMPL.BDS) - NROW(Pdata)

cleanPacFIN <- function(
  Pdata,
  keep_INPFC = lifecycle::deprecated(),
  keep_gears,
  keep_sample_type = c("", "M"),
  keep_sample_method = "R",
  keep_length_type,
  keep_age_method = NULL,
  keep_missing_lengths = lifecycle::deprecated(),
  keep_states = c("WA", "OR", "CA"),
  CLEAN = TRUE,
  spp = NULL,
  verbose = FALSE,
  savedir) {

  #### Deprecate old input arguments
  if (lifecycle::is_present(keep_INPFC)) {
    lifecycle::deprecate_stop(
      when = "0.0.1.0005",
      what = paste0("cleanPacFIN(keep_INPFC = )"),
      details = paste0(
        "It is thought that PSMFC areas can decipher much of what was\n",
        "previously determined with INPFC areas."
        )
      )
  }
  if (lifecycle::is_present(keep_missing_lengths)) {
    lifecycle::deprecate_stop(
      when = "0.0.1.0005",
      what = paste0("cleanPacFIN(keep_missing_lengths = )"),
      details = paste0(
        "All down-stream functionality works without filtering,\n",
        "but Pdata[is.na(Pdata[['length']]), ] can be used to filter them out."
        )
      )
  }

  #### CLEAN COLUMNS
  if ("PACFIN_SPECIES_CODE" %in% colnames(Pdata)) {
    Pdata <- cleanColumns(Pdata)
  }

  #### Fill in missing input arguments
  Pdata <- getGearGroup(Pdata, spp = spp, verbose = verbose)
  if (missing(keep_gears)) {
    keep_gears <- sort(unique(Pdata[, "geargroup"]))
  }
  Pdata[, "fleet"] <- match(Pdata$geargroup, keep_gears)
  if (missing(keep_length_type)) {
    keep_length_type <- sort(unique(c(Pdata[, "FISH_LENGTH_TYPE"],
      "", "A", "D", "F", "R", "S", "T", "U", NA)))
  }
  if (is.null(keep_age_method)) {
    keep_age_method <- unique(
      unlist(Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata))])
      )
  }

  #### Column names
  for (i in c("fishery", "UNK_WT")) {
    if (!i %in% colnames(Pdata)) {
      Pdata[, i] <- switch(i,
        fishery = 1,
        UNK_WT = NA)
    } # End if
  } # End for
  Pdata$fishyr <- Pdata$SAMPLE_YEAR
  Pdata$year <- Pdata$SAMPLE_YEAR
  if (!missing(savedir)) {
    png(filename = file.path(savedir, "PacFIN_comp_season.png"))
    on.exit(dev.off(), add = TRUE)
  }
  Pdata <- getSeason(Pdata, verbose = verbose,
    plotResults = !missing(savedir))

  #### Areas
  Pdata <- getState(Pdata, verbose = verbose,
    source = ifelse("AGID" %in% colnames(Pdata), "AGID", "SOURCE_AGID"))
  # California doesn't record SAMPLE_TYPE so we assume they are all Market samples
  Pdata[Pdata$state == "CA" & is.na(Pdata$SAMPLE_TYPE), "SAMPLE_TYPE"] <- "M"

  #### Sex
  Pdata[, "SEX"] <- getSex(data.vector = Pdata[, "SEX"], verbose = verbose)

  #### Lengths
  Pdata[, "length"] <- getLength(Pdata, verbose = verbose,
    keep = keep_length_type)
  Pdata[, "lengthcm"] <- floor(Pdata[, "length"] / 10)

  #### Age (originally in cleanAges)
  Pdata[, "age"] <- getAge(Pdata, verbose = verbose,
    keep = keep_age_method)

  #### Bad samples
  # Remove bad OR samples
  Pdata$SAMPLE_TYPE[Pdata$SAMPLE_NO %in% paste0("OR", badORnums)] <- "S"
  # Via Chantel, from Ali at ODFW, do not keep b/c they don't have exp_wt or FTID
  if ("SAMPLE_QUALITY" %in% colnames(Pdata)) {
    Pdata[Pdata[["SAMPLE_QUALITY"]] == 63, "SAMPLE_TYPE"] <- "S"
  }

  # Remove lengths and ages for gears we don't want
  Pdata[!Pdata[, "geargroup"] %in% keep_gears, "length"] <- NA
  Pdata[!Pdata[, "geargroup"] %in% keep_gears, "age"] <- NA

  #### Sex-specific _WGT and _NUM
  # Calculate sex-specific weights and numbers
  sw <- stats::ave(
    measurements::conv_unit(Pdata$FISH_WEIGHT, from = "g", to = "lbs"),
    Pdata$SEX, Pdata$SAMPLE_NO,
    FUN = sum)
  sn <- stats::ave(Pdata$FISH_WEIGHT, Pdata$SEX, Pdata$SAMPLE_NO,
    FUN = function(x) sum(!is.na(x)))
  Pdata[, "MALES_NUM"] <- ifelse(
    is.na(Pdata[, "MALES_NUM"]) & Pdata[, "SEX"] == "M",
    sn, Pdata[, "MALES_NUM"])
  Pdata[, "FEMALES_NUM"] <- ifelse(
    is.na(Pdata[, "FEMALES_NUM"]) & Pdata[, "SEX"] == "F",
    sn, Pdata[, "FEMALES_NUM"])
  Pdata[, "MALES_WGT"] <- ifelse(
    is.na(Pdata[, "MALES_WGT"]) & Pdata[, "SEX"] == "M",
    sw, Pdata[, "MALES_WGT"])
  Pdata[, "FEMALES_WGT"] <- ifelse(
    is.na(Pdata[, "FEMALES_WGT"]) & Pdata[, "SEX"] == "F",
    sw, Pdata[, "FEMALES_WGT"])
  Pdata[, "UNK_WT"] <- ifelse(
    is.na(Pdata[, "UNK_WT"]) & Pdata[, "SEX"] == "U",
    sw, Pdata[, "UNK_WT"])
  Pdata$MALES_WGT[is.na(Pdata$MALES_NUM) & Pdata$MALES_WGT == 0] <- NA
  Pdata$FEMALES_WGT[is.na(Pdata$FEMALES_NUM) & Pdata$FEMALES_WGT == 0] <- NA
  Pdata$UNK_WT[is.na(Pdata$UNK_NUM) & Pdata$UNK_WT == 0] <- NA

  #### Summary and return
  # bad records: keep TRUEs
  bad <- Pdata[, 1:2]
  bad[, "goodarea"] <- is.na(getArea(Pdata, verbose = verbose))
  bad[, "goodstype"] <- Pdata$SAMPLE_TYPE %in% keep_sample_type
  bad[, "goodsmeth"] <- Pdata$SAMPLE_METHOD %in% keep_sample_method
  bad[, "goodsno"] <- !is.na(Pdata$SAMPLE_NO)
  bad[, "goodstate"] <- Pdata[, "state"] %in% keep_states
  bad[, "keep"] <- apply(bad[, grep("^good", colnames(bad))], 1, all)
  # Move this check to after the keep to only report based on kept records
  ORsw <- is.na(Pdata[bad$keep, "EXP_WT"]) & Pdata[bad$keep, "state"] == "OR"
  CAsw <- is.na(Pdata[bad$keep, "SPECIES_WGT"]) & Pdata[bad$keep, "state"] == "CA"

  # Report removals
  if (verbose) {
    message("\n")
    message("N SAMPLE_TYPEs changed from M to S",
      " for special samples from OR: ",
      sum(Pdata$SAMPLE_NO %in% paste0("OR", badORnums)))
    message("N not in keep_sample_type (SAMPLE_TYPE): ",
      sum(!bad[, "goodstype"]))
    message("N not in keep_sample_method (SAMPLE_METHOD): ",
      sum(!bad[, "goodsmeth"]))
    message("N with SAMPLE_NO of NA: ",
      sum(!bad[, "goodsno"]))
    message("N without length: ", sum(is.na(Pdata$length)))
    message("N without age: ", sum(is.na(Pdata$age)))
    message("N without length and age: ", sum(is.na(Pdata$length) | is.na(Pdata$age)))
    message("N records: ", NROW(Pdata))
    message("N remaining if CLEAN: ", sum(bad[, "keep"]))
    message("N records will be removed if CLEAN: ", NROW(Pdata) - sum(bad[, "keep"]))
    message("The following are not removed with CLEAN ...")
    message("N sample weights not available for OR: ", sum(ORsw))
    message("N sample weights not available for CA: ", sum(CAsw))
  }

  if (!missing(savedir)) {
    plotCleaned(Pdata, savedir = savedir)
  }

  Pdata[, "CLEAN"] <- bad[, "keep"]
  if (CLEAN) {
    Pdata <- Pdata[bad[, "keep"], ]
  }

  if (!missing(savedir)) {
    wlpars <- getWLpars(Pdata, verbose = FALSE)
    utils::write.table(wlpars, sep = ",",
      row.names = TRUE, col.names = TRUE,
      file = file.path(savedir, "PacFIN_WLpars.csv"))
    if (verbose) {
      message("WL parameter estimates: see 'PacFIN_WLpars.csv'\n",
        "If some rows are NA, consider setting ALL of them individually\n",
        "'getExpansion_1('fa' = , 'fb' = , 'ma' = , ...)")
    }
  }

  return(Pdata)
} # End cleanPacFIN
