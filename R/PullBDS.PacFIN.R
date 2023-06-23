#' Pull biological data from the PacFIN database
#'
#' Pull biological data from the Comprehensive Biological Data
#' (comprehensive_bds_comm) table in the PacFIN database (PACFIN_MARTS). Data
#' are in their raw form and should be cleaned either by hand or using
#' [cleanPacFIN()].
#'
#' @inheritParams sql
#' @inheritParams getDB
#' @template savedir
#' @template verbose
#'
#' @return
#' An RData object is saved to the disk and the pulled data
#' are returned as an [invisible()] data frame.
#' The saved data can be read back in using [load()], but note that
#' upon loading, the object will be named `bds.pacfin`, which is its name
#' inside of the `.RData` file, and thus,
#' the object will retain this name within your work space.
#'
#' @export
#' @author John R. Wallace and Kelli F. Johnson
#'
#' @details
#' ## Data structure
#' Upon downloading, the data are changed from a long table to a wide table
#' using the combination of unique `FISH_ID` and `AGE_SEQUENCE_NUMBER`. This
#' change from long to wide allows for rows equating to a single fish with
#' columns containing information about all measurements for that fish. Multiple
#' age reads and information about those reads such as age reader will be in the
#' columns. The age read number, e.g., 1, 2, 3, 4, ..., is pasted onto the
#' column name separated by an underscore. So, the maximum number you see is the
#' maximum number of times an otolith was read in your data set. Not all double
#' reads are currently available within PacFIN and users should contact the
#' ageing labs if they wish to inform ageing-error matrices.
#'
#' `AGE_COUNT` is a somewhat cryptic column name and does not always make sense
#' when compared to `AGE_SEQUENCE_NUMBER`. It was determined that the former is
#' useful to identify how many potential agers were exposed to this fish.
#' For example, if `AGE_SEQUENCE_NUMBER` has a maximum value of three for a
#' given `FISH_ID`, then you can expect `AGE_COUNT` to be three for all three
#' rows in the PacFIN database for that fish. This is not always true though.
#' Sometimes, not all `AGE_SEQUENCE_NUMBER`s are present and they can skip
#' numbers for a given `FISH_ID`, and in this case, `AGE_COUNT` will be the
#' maximum `AGE_SEQUENCE_NUMBER` for a given `FISH_ID`.
#'
#' `FINAL_FISH_AGE_IN_YEARS` is known as the best age for a given fish.
#' This will not always match an age reader or be a number determinable
#' from the individual age reads in `AGE_IN_YEARS`. Patrick explained to me
#' that when age reads do not agree, particularly for younger fish, then
#' the senior reader will work together with the junior reader to determine
#' an agreed-upon age. Other times, the senior reader's value will always
#' be used, or it could be that together they determine that they were both
#' wrong and a new age is proposed as the `resolved age`. Nevertheless,
#' it can be quite messy and there is no way to predict the best age.
#'
#' `FISH_WEIGHT_GUTTED` is typically only available for a small subset of
#' samples that were sampled "purposively" by Washington state. E.g., if a
#' fish is weighed whole and then headed and gutted and weighed again, then
#' there would be two rows with the same `FISH_ID` but different `FISH_WEIGHT`
#' entries in the PacFIN BDS table. The downloaded data are reshaped such that
#' this second gutted weight is placed in `FISH_WEIGHT_GUTTED` and the fish is
#' represented in a single row. Granted, these purposive samples should not be
#' used in an assessment of the population status but they are included in the
#' download for completeness.
#'
#' ## Searching for species
#' Values passed to `pacfin_species_code` are searched for using regular
#' expression matching, which is different than the exact matching that is done
#' is [PullCatch.PacFIN()]. The use of pattern matching allows for species codes
#' with mistakes like leading and trailing spaces to be found. This is doable in
#' the biological data because data for nominal species codes are few. In my
#' experiences these mistakes in the species codes are more common for PacFIN
#' species codes that are three letters rather than the standard four letters.
#'
#' @seealso
#' * [cleanColumns()] to change to legacy column names
#' * [cleanPacFIN()] to manipulate and subset the returned object
#'
#' @examples
#' \dontrun{
#' # You will be asked for your password
#' pd <- PullBDS.PacFIN(pacfin_species_code = "POP")
#' }
#'
PullBDS.PacFIN <- function(pacfin_species_code,
                           username = getUserName("PacFIN"),
                           password = ask_password(),
                           savedir = getwd(),
                           verbose = TRUE) {
  # Input checks
  stopifnot(
    "`verbose` must be a logical." =
    is.logical(verbose) &&
    length(verbose) == 1
  )
  file_species_code <- paste(pacfin_species_code, collapse = "--")

  # Pull from PacFIN
  if (verbose) {
    message("Pulling BDS data from PacFIN for ", pacfin_species_code)
  }
  data_raw <- getDB(
    sql = sql_bds(pacfin_species_code),
    username = username,
    password = password
  )

  # Checks on data_raw
  # stop calls
  stopifnot("No data found" = NROW(data_raw) > 0)

  # message calls
  if (verbose) {
    message(
      "\nThe following PACFIN_SPECIES_CODE(s) were found:\n",
      paste0(
        utils::capture.output(
          dplyr::count(data_raw, PACFIN_SPECIES_CODE) %>%
            dplyr::mutate(PACFIN_SPECIES_CODE = sQuote(PACFIN_SPECIES_CODE))
        ),
        collapse = "\n"
      ),
      "\n"
    )
  }

  # warning calls
  sample_agency <- unique(data_raw[, "SAMPLE_AGENCY"])
  if (verbose && !all(is.na(sample_agency))) {
    warning(
      call. = FALSE,
      immediate. = TRUE,
      "SAMPLE_AGENCY includes non-NULL values and should be left in the\n",
      "pulled data frame; please contact the maintainer and note that\n",
      "SAMPLE_AGENCY == ",
      glue::glue_collapse(sample_agency, sep = ", ", last = " and "),
      "\n"
    )
  }
  rm(sample_agency)
  fish_id <- is.na(data_raw[["FISH_ID"]])
  if (verbose && sum(fish_id) > 0) {
    warning(
      call. = FALSE,
      immediate. = TRUE,
      "FISH_ID includes NULL(s) for ", sum(fish_id), " rows.\n",
      "These rows have been removed from the data; but you should contact\n",
      "state representatives for ",
      glue::glue_collapse(
        unique(data_raw[fish_id, "AGENCY_CODE"]),
        sep = ", ",
        last = " and "
      ),
      "to let them know.\n"
    )
  }
  rm(fish_id)
  # Check that WA purposive samples only have duplicated information in
  # FISH_WEIGHT (because they will be made wider later)
  fish_id <- duplicated(data_raw[, c("FISH_ID", "AGE_SEQUENCE_NUMBER")])
  data_fish <- data_raw %>%
    dplyr::filter(
      AGENCY_CODE == "W",
      SAMPLE_METHOD_CODE == "P",
      FISH_ID %in% data_raw[fish_id, "FISH_ID"]
    ) %>%
    dplyr::group_by(FISH_ID)
  if (NROW(data_fish)) {
    check <- data_fish %>%
      dplyr::select_if(~ !all(is.na(.))) %>%
      dplyr::select(-BDS_ID, -FISH_WEIGHT) %>%
      dplyr::group_map(
        .f = ~ duplicated(.x)
      ) %>%
      do.call(what = "rbind")
    if (!all(check[, 2])) {
      stop(
        call. = FALSE,
        "There were records for Washington sampled purposively that contain\n",
        "unique information, beyond `FISH_WEIGHT`, per row. Please contact\n",
        "the package maintainer to accommodate these samples."
      )
    }
  }
  rm(fish_id, data_fish)
  # Check for duplicated FISH_IDs
  fish_id <- duplicated(data_raw[, c("FISH_ID", "AGE_SEQUENCE_NUMBER")]) &
    (data_raw[["SAMPLE_METHOD_CODE"]] != "P" & data_raw[["AGENCY_CODE"]] != "W")
  if (verbose && sum(fish_id)) {
    warning(
      call. = FALSE,
      immediate. = TRUE,
      noBreaks. = FALSE,
      "The downloaded data contains duplicated entries that will be\n",
      "removed prior to returning the data. Please notify the agency that\n",
      "provided the following duplicated samples:"
    )
    data_raw[fish_id, ] %>%
      dplyr::group_by(AGENCY_CODE, SAMPLE_YEAR, SAMPLE_NUMBER) %>%
      dplyr::count() %>%
      print(n = sum(fish_id))
  }
  rm(fish_id)

  # todo: think about not changing column names
  data <- data_raw %>%
    # Fix the data
    dplyr::filter(!is.na(FISH_ID)) %>%
    # Move duplicated FISH_WEIGHT to FISH_WEIGHT_GUTTED from WA and remove
    # FISH_IDs that are duplicated for CA
    dplyr::group_by(FISH_ID) %>%
    dplyr::mutate(
      weight_type = ifelse(
        test = FISH_WEIGHT == max(FISH_WEIGHT),
        yes = "FISH_WEIGHT",
        no = "FISH_WEIGHT_GUTTED"
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = !dplyr::matches("BDS_ID"),
      names_from = "weight_type",
      values_from = "FISH_WEIGHT",
      values_fn = list(FISH_WEIGHT = unique)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(FISH_WEIGHT, .after = FORK_LENGTH_IS_ESTIMATED) %>%
    dplyr::select(-"NA") %>%
    # Continue fixing the data
    dplyr::select(-SAMPLE_AGENCY) %>%
    dplyr::mutate(
      FISH_LENGTH_TYPE_CODE = ifelse(
        test = FISH_LENGTH_TYPE_CODE != FALSE,
        as.character(FISH_LENGTH_TYPE_CODE),
        "F"
      ),
      AGE_SEQUENCE_NUMBER = tidyr::replace_na(AGE_SEQUENCE_NUMBER, 1)
    ) %>%
    # Do some renaming of columns
    dplyr::rename(age = dplyr::matches("^AGE_IN_YEARS")) %>%
    dplyr::rename(agedby = dplyr::matches("PERSON_WHO_AGED")) %>%
    dplyr::rename(AGE_METHOD = dplyr::matches("AGE_METHOD_CODE"))

  # Long to wide to facilitate estimating ageing error
  # Multiple BDS_IDs can pertain to a single FISH_ID
  # each BDS_ID is an age read
  # identical across rows: SAMPLE_ID, SAMPLE_NO, FISH_ID
  # unique across rows: BDS_ID, AGE_ID, AGE_SEQUENCE_NUMBER
  bds.pacfin <- data %>%
    tidyr::pivot_wider(
      id_cols = !dplyr::matches("BDS_ID"),
      names_from = AGE_SEQUENCE_NUMBER,
      values_from = c(
        AGE_ID,
        AGE_METHOD:AGENCY_AGE_STRUCTURE_CODE
      ),
      names_sort = TRUE,
      names_glue = "{.value}{AGE_SEQUENCE_NUMBER}",
      values_fill = NA
    )
  # Short check b/c pivot_wider can make lists
  if (!class(bds.pacfin[["age1"]]) %in% c("integer", "logical")) {
    stop(
      call. = FALSE,
      "pivot_wider failed to transform age reads to a wide data frame!"
    )
  } else {
    # TODO: Think about returning a tibble rather than a data frame
    bds.pacfin <- data.frame(bds.pacfin)
  }

  # Save appropriate summaries
  savefn <- file.path(savedir,
    paste(
      sep = ".",
      "PacFIN",
      file_species_code,
      "bds",
      format(Sys.Date(), "%d.%b.%Y"),
      "RData"
    )
  )
  save(bds.pacfin, file = savefn)

  return(invisible(bds.pacfin))
}
