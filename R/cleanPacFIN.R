#' Clean raw PacFIN data
#'
#' Clean raw PacFIN data to remove unsuitable samples if `CLEAN = TRUE` and
#' convert units of measured quantities to work with downstream functions.
#' Raw data are meant to be inclusive of everything from PacFIN so users can
#' explore all that is available, but this means that raw data will **ALWAYS**
#' include information that is not appropriate for use in
#' US West Coast stock assessments.
#'
#' @template Pdata
#' @param keep_INPFC *Deprecated*. Areas are now defined using different methods.
#' @param keep_gears A character vector including only the gear types you want
#' to label as unique fleets. Order matters and will define fleet numbering.
#' If the argument is missing, which is the default, then all found gear groups
#' are maintained and ordered alphabetically. For more details see
#' [getGearGroup] that lists a web link for where you can find the
#' available gear groupings and how they link to `"GRID"` within your data.
#' GRID is a legacy term from PacFIN, now identified as
#' PACFIN_GEAR_CODE in the biological and fish ticket data, where
#' GR is short for gear and
#' ID is short for identification.
#' Typical entries will include character values such as `HKL`, `POT`, `TWL`,
#' where the latter is short for all non-shrimp trawls and
#' `TWS` is shrimp trawls.
#' Other gear identification codes and their definitions include
#' `DRG` which is dredge gear,
#' `MSC` which is all other miscellaneous gear such as diving or river trawls,
#' `NET` which is all non-trawl net gear,
#' `NTW` which is non-trawl gear, and
#' `TLS` which is trolling gear.
#' As a special case, `MID` is available for spiny dogfish to extract
#' mid-water trawl data as a separate fleet.
#' @param keep_sample_type A vector of character values specifying the types of
#' samples you want to keep. The default is to keep `c("M")`. Available
#' types include market (M), research (R), special request (S), and
#' commercial on-board (C). There are additional samples without a `SAMPLE_TYPE`,
#' but they are only kept if you include `NA` in your call.
#' All sample types from California are assigned to `M`.
#' Including commercial on-board samples is not recommended because
#' they might also be in WCGOP data and would lead to double counting.
#' @param keep_sample_method A vector of character values specifying the types of
#' sampling methods you want to keep. The default is to keep \code{"R"}, which
#' refers to samples that were sampled randomly. Available types include
#' random (R), stratified (S), systematic (N), purposive (P), and special (X).
#' As of February 17, 2021,
#' Washington is the only state with a sample type of `""`, and it was limited
#' to two special samples of yelloweye rockfish.
#' @param keep_length_type A vector of character values specifying the types of
#' length samples to keep. There is no default value, though users will typically
#' want to keep `c("", "F", "A")`, but should also think about using
#' `c("", "F", "A", NA)`. Note that types other than those listed below
#' can be present, especially if you are dealing with a skate.
#' `A` is alternate length,
#' `D` is dorsal length,
#' `F` is fork length,
#' `S` is standard length, and
#' `T` is total length.
#' @param keep_age_method A vector of ageing methods to retain in the data. All fish
#' aged with methods other than those listed will no longer be considered aged.
#' A value of `NULL`, the default, will keep all ageing methods. However,
#' a vector of `c("B", "BB", S", "", NA, 1, 2)` will keep all unaged fish and those
#' that were aged with break and burn and surface reads. You do not really need
#' to include such a verbose vector of values though because numbers are converted
#' to appropriate character codes in [getAge]. Therefore, something like
#' `c("B", "S")` would be sufficient to keep all break and burn and surface reads.
#' @param keep_missing_lengths *Deprecated*. Just subset them using
#' `is.na(Pdata[, 'length']) after running `cleanPacFIN` if you want to remove
#' lengths, though there is no need because the package accommodates keeping them in.
#' @param keep_states A vector of states that you want to keep, where each state
#' is defined using a two-letter abbreviation, e.g., `WA`. The default is to keep
#' data from all three states, `keep_states = c("WA", "OR", "CA")`.
#' Add `'UNK'` to the vector if you want to keep data not assigned to a state.
#' @param CLEAN A logical value used when you want to remove data from the input
#' data set. The default is `TRUE`. Where the opposite returns the original
#' data with additional columns and reports on what would have been removed.
#' @template spp
#' @template verbose
#' @template savedir
#'
#' @export
#' @return The input data filtered for desired areas and record types
#' specified, with added columns
#'
#' * year: initialized from SAMPLE_YEAR\cr
#' * fleet: initialized to 1
#' * fishery: initialized to 1
#' * season: initialized to 1.  Change using [getSeason]
#' * state: initialized from SOURCE_AGID.  Change using [getState]
#' * length: length in mm, where `NA` indicates length is not available
#' * lengthcm: floored cm from FORK_LENGTH when available, otherwise FISH_LENGTH
#' * geargroup: the gear group associated with each [GRID](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' * weightkg: fish weight in kg from FISH_WEIGHT and FISH_WEIGHT_UNITS
#'
#' @details
#' The original fields in the returned data are left untouched,
#' with the exception of
#' * `SEX`: modified using [nwfscSurvey::codify_sex()] and upon return will
#' only include character values such that fish with an unidentified sex are
#' now `"U"`.
#' * Age: the best ages to use going forward rather than just the first age read.
#'
#' \subsection{CLEAN}{
#' The data are put through various tests before they are returned
#' and the results of these tests are stored in the \code{CLEAN} column.
#' Thus, sometimes it is informative to run \code{cleanPacFIN(CLEAN = FALSE)}
#' and use frequency tables to inspect which groups of data will be removed
#' from the data set when you change the code to be \code{CLEAN = FALSE}.
#' For example, many early length compositions do not have information on
#' the weight of fish that were sampled, and thus, there is no way to infer
#' how much the entire sample weighed or how much the tow/trip weighed.
#' Therefore, these data cannot be expanded and are removed using
#' \code{CLEAN = TRUE}. Some stock assessment authors or even previous
#' versions of this very code attempted to use adjacent years to inform
#' weights. The number of assumptions for this was great and state
#' representatives discouraged inferring data that did not exist.
#' }
#'
#' \subsection{Furthermore}{
#' The values created as new columns are for use by other functions in this package.
#' In particular, `fishyr` and `season` are useful if there are multiple
#' seasons (e.g., winter and summer, as in the petrale sole assessment), and the
#' year is adjusted so that "winter" occurs in one year, rather than across two.
#'
#' The `fleet`, `fishery`, and `state` columns are meant for use in
#' stratifying the data according to the particulars of an assessment.
#' }
#'
#' @seealso [getState], [getSeason]
#'
#' @author Andi Stephens

cleanPacFIN <- function(Pdata,
                        keep_INPFC = lifecycle::deprecated(),
                        keep_gears,
                        keep_sample_type = c("M"),
                        keep_sample_method = "R",
                        keep_length_type,
                        keep_age_method = NULL,
                        keep_missing_lengths = lifecycle::deprecated(),
                        keep_states = c("WA", "OR", "CA"),
                        CLEAN = TRUE,
                        spp = NULL,
                        verbose = TRUE,
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
  if (check_columns_downloaded(Pdata)) {
    Pdata <- cleanColumns(Pdata)
  }
  check_calcom <- any(Pdata[["SOURCE_AGID"]] == "CalCOM")

  #### Fill in missing input arguments
  Pdata <- getGearGroup(Pdata, spp = spp, verbose = verbose)
  if (missing(keep_gears)) {
    keep_gears <- sort(unique(Pdata[, "geargroup"]))
  }
  Pdata[, "fleet"] <- match(Pdata$geargroup, keep_gears)
  if (missing(keep_length_type)) {
    keep_length_type <- sort(unique(c(
      Pdata[, "FISH_LENGTH_TYPE"],
      "", "A", "D", "F", "R", "S", "T", "U", NA
    )))
  }
  if (is.null(keep_age_method)) {
    keep_age_method <- unique(
      unlist(Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata))])
    )
  }

  #### Column names
  if (!"fishery" %in% colnames(Pdata)) {
    Pdata[, "fishery"] <- 1
  }
  Pdata$fishyr <- Pdata$SAMPLE_YEAR
  Pdata$year <- Pdata$SAMPLE_YEAR
  if (!missing(savedir)) {
    grDevices::png(filename = file.path(savedir, "PacFIN_comp_season.png"))
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  }
  Pdata <- getSeason(Pdata,
    verbose = verbose,
    plotResults = !missing(savedir)
  )

  #### Areas
  Pdata <- getState(Pdata,
    verbose = verbose,
    source = ifelse("AGID" %in% colnames(Pdata), "AGID", "SOURCE_AGID")
  )
  # California doesn't record SAMPLE_TYPE so we assume they are all Market samples
  Pdata[Pdata$state == "CA" & is.na(Pdata$SAMPLE_TYPE), "SAMPLE_TYPE"] <- "M"

  #### Sex
  Pdata[, "SEX"] <- nwfscSurvey::codify_sex(Pdata[, "SEX"])

  #### Lengths
  Pdata[, "length"] <- getLength(Pdata,
    verbose = verbose,
    keep = keep_length_type
  )
  Pdata[, "lengthcm"] <- floor(Pdata[, "length"] / 10)

  #### Age (originally in cleanAges)
  # Named to "Age" to match nwfscSurvey where Age is used.
  Pdata[, "Age"] <- getAge(
    Pdata,
    verbose = verbose,
    keep = keep_age_method
  )
  Pdata[, "age_method"] <- getAgeMethod(Pdata)

  #### Weight (random units in)
  Pdata[, "weightkg"] <- getweight(
    length = NULL,
    weight = Pdata[["FISH_WEIGHT"]],
    unit.in = Pdata[["FISH_WEIGHT_UNITS"]],
    unit.out = "kg"
  )

  #### Bad samples
  # Remove bad OR samples
  Pdata$SAMPLE_TYPE[Pdata$SAMPLE_NO %in% paste0("OR", badORnums)] <- "S"
  # Via Chantel, from Ali at ODFW, do not keep b/c they don't have exp_wt or FTID
  if ("SAMPLE_QUALITY" %in% colnames(Pdata)) {
    Pdata[Pdata[["SAMPLE_QUALITY"]] == 63, "SAMPLE_TYPE"] <- "S"
  }

  #### Summary and return
  # bad records: keep TRUEs
  bad <- Pdata[, 1:2]
  bad[, "goodarea"] <- is.na(getArea(Pdata, verbose = verbose))
  bad[, "goodstype"] <- Pdata$SAMPLE_TYPE %in% keep_sample_type
  bad[, "goodsmeth"] <- Pdata$SAMPLE_METHOD %in% keep_sample_method
  bad[, "goodsno"] <- !is.na(Pdata$SAMPLE_NO)
  bad[, "goodstate"] <- Pdata[, "state"] %in% keep_states
  bad[, "goodgear"] <- Pdata[, "geargroup"] %in% keep_gears
  bad[, "goodEXP_WT"] <- !(is.na(Pdata[["EXP_WT"]]) & Pdata[["state"]] == "OR")
  bad[, "keep"] <- apply(bad[, grep("^good", colnames(bad))], 1, all)

  # Report removals
  if (verbose) {
    message("\n")
    message(
      "N SAMPLE_TYPEs changed from M to S",
      " for special samples from OR: ",
      sum(Pdata$SAMPLE_NO %in% paste0("OR", badORnums))
    )
    message(
      "N not in keep_sample_type (SAMPLE_TYPE): ",
      sum(!bad[, "goodstype"])
    )
    message("N with SAMPLE_TYPE of NA: ", sum(is.na(Pdata[["SAMPLE_TYPE"]])))
    message(
      "N not in keep_sample_method (SAMPLE_METHOD): ",
      sum(!bad[, "goodsmeth"])
    )
    message(
      "N with SAMPLE_NO of NA: ",
      sum(!bad[, "goodsno"])
    )
    message("N without length: ", sum(is.na(Pdata$length)))
    message("N without Age: ", sum(is.na(Pdata$Age)))
    message("N without length and Age: ", sum(is.na(Pdata$length) | is.na(Pdata$Age)))
    message("N sample weights not available for OR: ", sum(!bad[, "goodEXP_WT"]))
    message("N records: ", NROW(Pdata))
    message("N remaining if CLEAN: ", sum(bad[, "keep"]))
    message("N removed if CLEAN: ", NROW(Pdata) - sum(bad[, "keep"]))
    if (check_pacfin_species_code_calcom(Pdata$SPID)) {
      if (check_calcom) {
        cli::cli_alert_success(
          "Data are from a flatfish and CalCOM data are present"
        )
      } else {
        cli::cli_alert_danger(
          "Data are from a flatfish but no CalCOM data are present, check with E.J."
        )
      }
    }
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
    utils::write.table(wlpars,
      sep = ",",
      row.names = FALSE, col.names = TRUE,
      file = file.path(savedir, "PacFIN_WLpars.csv")
    )
    if (verbose) {
      message(
        "WL parameter estimates: see 'PacFIN_WLpars.csv'\n",
        "If some rows are NA, consider setting ALL of them individually\n",
        "'getExpansion_1('fa' = , 'fb' = , 'ma' = , ...)"
      )
    }
  }

  return(Pdata)
}
