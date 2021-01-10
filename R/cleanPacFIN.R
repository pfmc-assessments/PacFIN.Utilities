#' Clean raw PacFIN biological data
#'
#' Clean raw data from PacFIN to remove unsuitable samples and
#' convert units to those appropriate for the rest of the code.
#'
#' @export
#'
#' @template Pdata
#' @param keep_INPFC A vector of character values specifying which INPFC areas
#' to keep. See `[ls_INPFC](area = "ALL")` for a vector of options.
#' Note that PacFIN has some errors in this column, e.g., Columbia is listed
#' as both CL and COL. For an internally generated list of areas within the
#' lower 48 of the United States see \code{\link{ls_INPFC}(area = "US")}.
#' The default is to keep all areas.
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
#' @param keep_missing_lengths A logical value used when you want to subset the data
#' for ages, i.e., keep all missing lengths because you care about age data that
#' might not have an associated length. By default, missing lengths are removed,
#' i.e., \code{keep_missing_lengths = FALSE}, but they are only removed if
#' \code{CLEAN = TRUE}.
#' @param keep_states A vector of states that you want to keep, where each state
#' is defined using a two-letter abbreviation, e.g., `WA`. The default is to keep
#' data from all three states, even though California data often do not
#' have information on sample type, sample method, or INPFC area. The default,
#' `keep_states = c("WA", "OR", "CA")`, is to keep the data and add California to
#' `keep_INPFC`. Add `'UNK'` to the vector if you want to keep data not assigned
#' to a state.
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
#' with the exception of SEX, which is modified so that unidentified fish are labeled
#' "U".
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
  keep_INPFC,
  keep_gears,
  keep_sample_type = c("", "M"),
  keep_sample_method = "R",
  keep_length_type,
  keep_age_method = NULL,
  keep_missing_lengths = FALSE,
  keep_states = c("WA", "OR", "CA"),
  CLEAN = TRUE,
  spp = NULL,
  verbose = FALSE,
  area = "US",
  savedir) {

  #### Fill in missing inputs

  # If keep_INPFC input missing automatically check to see what
  # records are from the area specified (area = US as the default) 
  if (missing(keep_INPFC)) {
    keep_INPFC <- ls_INPFC(area = area)
  }
  # Get geargroups based on GRID column
  Pdata <- getGearGroup(Pdata, spp = spp, verbose = verbose)
  if (missing(keep_gears)) {
    keep_gears <- sort(unique(Pdata[, "geargroup"]))
  }
  Pdata[, "fleet"] <- match(Pdata$geargroup, keep_gears)

  # Check to see FISH_LENGTH_TYPE is NA or FALSE and correct to F (fork)
  if (all(Pdata[, "FISH_LENGTH_TYPE"] %in% c(FALSE, NA))) {
    Pdata[, "FISH_LENGTH_TYPE"] <- as.character("F")
  }
  if (missing(keep_length_type)) {
    keep_length_type <- sort(unique(c(Pdata[, "FISH_LENGTH_TYPE"],
      "", "A", "D", "F", "R", "S", "T", "U", NA)))
  }
  if (is.null(keep_age_method)) {
    keep_age_method <- unique(
      unlist(Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata))])
      )
  } else {
    if ("B" %in% keep_age_method) keep_age_method <- c(keep_age_method, 1)
    if ("S" %in% keep_age_method) keep_age_method <- c(keep_age_method, 2)
  }
  if ("CA" %in% keep_states) {
    keep_INPFC <- c(keep_INPFC, "CALCOM")
  }

  #### Column names
  for (i in c("fishery", "age", "INPFC_AREA", "UNK_WT")) {
    if (!i %in% colnames(Pdata)) {
      Pdata[, i] <- switch(i,
        fishery = 1,
        age = NA,
        INPFC_AREA = NA,
        UNK_WT = NA)
    } # End if
  } # End for
  Pdata$fishyr <- Pdata$SAMPLE_YEAR
  Pdata$year <- Pdata$SAMPLE_YEAR

  # Is savedir specified create data figure
  if (!missing(savedir)) {
    png(filename = file.path(savedir, "PacFIN_comp_season.png"))
    on.exit(dev.off(), add = TRUE)
  }
  Pdata <- getSeason(Pdata, verbose = verbose,
    plotResults = !missing(savedir))

  #### Areas
  Pdata[, "INPFC_AREA"] <- gsub("\\s", "", Pdata[, "INPFC_AREA"])
  Pdata[, "INPFC_AREA"] <- gsub("^COL$", "CL", Pdata[, "INPFC_AREA"])
  Pdata[, "INPFC_AREA"] <- gsub("^VUS$", "VN", Pdata[, "INPFC_AREA"])
  Pdata <- getState(Pdata, verbose = verbose,
    source = ifelse("AGID" %in% colnames(Pdata), "AGID", "SOURCE_AGID"))
  if ("CA" %in% keep_states) {
    Pdata[Pdata$state == "CA" & is.na(Pdata$SAMPLE_TYPE), "SAMPLE_TYPE"] <- "M"
    Pdata[Pdata$state == "CA" & is.na(Pdata$SAMPLE_METHOD), "SAMPLE_METHOD"] <- "R"
    Pdata[Pdata$state == "CA" & is.na(Pdata$INPFC_AREA), "INPFC_AREA"] <- "CALCOM"
  }

  #### Sex
  Pdata[is.na(Pdata[, "SEX"]), "SEX"] <- "U"
  Pdata[Pdata[, "SEX"] == 0, "SEX"] <- "U"

  #### Lengths
  # Use FISH_LENGTH if there is no FORK_LENGTH.
  width2length <- convertlength_skate(Pdata, returntype = "estimated")

  if (!is.null(spp) && spp %in% c("hake", "pacific hake")) {
    Pdata[, "FORK_LENGTH"] <- ifelse(is.na(Pdata[, "FORK_LENGTH"]),
      Pdata[, "FISH_LENGTH"], Pdata[, "FORK_LENGTH"])
    Pdata[, "FISH_LENGTH_UNITS"] <- ifelse(
      tolower(Pdata[, "FISH_LENGTH_UNITS"]) == "cm" & Pdata[, "FISH_LENGTH"] > 90,
      "MM",
      Pdata[, "FISH_LENGTH_UNITS"]
      )
  }
  # Fix FISH_LENGTH_TYPE if changed to TRUE FALSE because only entry is F
  # for fork length, which gets changed to a logical
  Pdata[, "FISH_LENGTH_TYPE"] <- ifelse(Pdata[, "FISH_LENGTH_TYPE"] != FALSE,
    as.character(Pdata[, "FISH_LENGTH_TYPE"]),
    "F")
  # If FISH_LENGTH_TYPE == F but FORK_LENGTH is NA replace it with FISH_LENGTH 
  Pdata$FORK_LENGTH = ifelse(Pdata$FISH_LENGTH_TYPE == "F" & is.na(Pdata$FORK_LENGTH),
    Pdata$FISH_LENGTH, Pdata$FORK_LENGTH)

  Pdata$length <- ifelse(Pdata$FISH_LENGTH_TYPE %in% c("", "A", "F", NA),
    Pdata$FORK_LENGTH, NA)
  if (all(Pdata$SPID %in% c("LSKT", "BSKT"))) {
    Pdata$length <- ifelse(
      # type "A" is associated with disc width for skates
      "A" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "A",
      width2length,
      Pdata$length)
  }
  Pdata$length <- ifelse(
    "D" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "D" &
    Pdata$FORK_LENGTH != Pdata$FISH_LENGTH,
      Pdata$FORK_LENGTH, Pdata$length)
  Pdata$length <- ifelse(
    # type "R" is associated with inter-spiracle width for skates (used by WDFW)
    "R" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "R",
    width2length,
    Pdata$length)
  Pdata$length <- ifelse(
    "S" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "S",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    "T" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "T",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    "U" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "U",
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length)
  Pdata$length <- ifelse(
    "" %in% keep_length_type & Pdata$FISH_LENGTH_TYPE == "",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    NA %in% keep_length_type & is.na(Pdata$FISH_LENGTH_TYPE),
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length)
  Pdata$length <- ifelse(Pdata$FISH_LENGTH_TYPE %in% keep_length_type,
    Pdata$length, NA)
  Pdata$length[Pdata$length == 0] <- NA
  goodtypes <- c("", "A", "D", "F", "R", "S", "T", "U", NA)
  if (any(!Pdata$FISH_LENGTH_TYPE %in% goodtypes)) {
    stop("cleanPacFIN can only accommodate the following FISH_LENGTH_TYPEs:\n",
      sprintf("'%s' ", goodtypes),
      "\nPlease contact the package maintainer to add additional types.")
  }

  # Convert mm to cm
  #if ("FISH_LENGTH_UNITS" %in% colnames(Pdata)) {
  #  Pdata$lengthcm <- floor(mapply(measurements::conv_unit,
  #    x = Pdata[, "length"],
  #    from = ifelse(is.na(Pdata[, "FISH_LENGTH_UNITS"]),
  #      "cm",
  #      tolower(Pdata[, "FISH_LENGTH_UNITS"])
  #      ),
  #    MoreArgs = list(to = "cm"))
  #  )
  # Moving to the approach below due to speed:
  # There can be a mixture of both cm and mm in a data set. Needs to convert
  # ones in mm to cm and keep the cm records as is.
  if ("FISH_LENGTH_UNITS" %in% colnames(Pdata)) {
    Pdata[,"lengthcm"] = ifelse(Pdata$FISH_LENGTH_UNITS %in% c("cm", "CM"),
      Pdata$length, NA)
    Pdata[,"lengthcm"] = ifelse(Pdata$FISH_LENGTH_UNITS %in% c("mm", "MM"),
      Pdata$length / 10, Pdata[,"lengthcm"]) 
  } else {
    if (verbose) message("Length assumed to be in mm.")
    Pdata[, "lengthcm"] <- floor(Pdata[, "length"] / 10)
  }
  
  #### Age (originally in cleanAges)
  if (!"FISH_AGE_YEARS_FINAL" %in% colnames(Pdata)) {
    Pdata$FISH_AGE_YEARS_FINAL <- NA
  }
  for (iiname in grep("AGE_METHOD[0-9]+$", colnames(Pdata), value = TRUE)) {
    iinum <- type.convert(as.is = TRUE, gsub("[a-zA-Z_]", "", iiname))
    Pdata[, paste0("age", iinum)] <- ifelse(
      Pdata[, paste0("AGE_METHOD", iinum)] %in% keep_age_method,
      Pdata[, paste0("age", iinum)],
      NA)
  }
  # Take the average of all non-zero or non-NA ages if multiple reads
  # are available
  Pdata[, "age"] <- ifelse(!is.na(Pdata$FISH_AGE_YEARS_FINAL),
    Pdata$FISH_AGE_YEARS_FINAL,
    apply(Pdata[, grep("age[0-9]+$", colnames(Pdata))], 1,
      FUN = function(x) {
        if (all(is.na(x))) return(NA)
        if (all(x == 0)) return(NA)
        return(mean(x[!x %in% c(0, NA)], na.rm = FALSE))
    })
    )
  Pdata[
    apply(
      Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata)), drop = FALSE],
      1, FUN = function(x) !any(x %in% keep_age_method)),
    "age"] <- NA

  #### Bad samples

  # Remove bad OR samples
  Pdata$SAMPLE_TYPE[Pdata$SAMPLE_NO %in% paste0("OR", badORnums)] <- "S"
  ORsw <- is.na(Pdata[, "EXP_WT"]) & Pdata[, "state"] == "OR"
  CAsw <- is.na(Pdata[, "SPECIES_WGT"]) & Pdata[, "state"] == "CA"

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
  # bad records
  bad <- Pdata[, 1:2]
  bad[, "goodINPFC"] <- checkINPFC(Pdata, keep = keep_INPFC)
  bad[, "goodstype"] <- Pdata$SAMPLE_TYPE %in% keep_sample_type
  bad[, "goodsmeth"] <- Pdata$SAMPLE_METHOD %in% keep_sample_method
  bad[, "goodsno"] <- !is.na(Pdata$SAMPLE_NO)
  bad[, "goodlen"] <- ifelse(keep_missing_lengths, TRUE, !is.na(Pdata$length))
  bad[, "goodstate"] <- Pdata[, "state"] %in% keep_states
  bad[, "keep"] <- apply(bad[, grep("^good", colnames(bad))], 1, all)

  # Report removals
  if (verbose) {
    message("cleanPacFIN Report")
    message("N records, N remaining if CLEAN: ",
      NROW(Pdata), ", ", sum(bad[, "keep"]))
    message("N not given a state (keep_states): ",
      sum(!bad[, "goodstate"]))
    message("N not in US INPFC and 'NA': ",
      sum(Pdata[, "INPFC_AREA"] %in% ls_INPFC(area = "US")))
    message("N not in 'keep_INPFC' (INPFC_AREA): ",
      sum(!bad[, "goodINPFC"]))
    message("N SAMPLE_TYPEs changed from M to S",
      " for special samples from OR: ",
      sum(Pdata$SAMPLE_NO %in% paste0("OR", badORnums)))
    message("N not in keep_sample_type (SAMPLE_TYPE): ",
      sum(!bad[, "goodstype"]))
    message("N not in keep_sample_method (SAMPLE_METHOD): ",
      sum(!bad[, "goodsmeth"]))
    message("N with SAMPLE_NO of NA: ",
      sum(!bad[, "goodsno"]))
    message("N with no usable length, age, or length and age: ",
      sum(is.na(Pdata$length)), ", ",
      sum(is.na(Pdata$age)), ", ",
      sum(is.na(Pdata$length) | is.na(Pdata$age))
      )
    message("N sample weights not available for OR or CA\n  ",
      "(note these are not removed with CLEAN): ",
      sum(ORsw), ", ", sum(CAsw))
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
