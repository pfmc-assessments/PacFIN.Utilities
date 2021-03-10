#' Pull biological data from the PacFIN database
#'
#' Pull biological data from the
#' Comprehensive Biological Data (comprehensive_bds_comm) table
#' in the PacFIN database (PACFIN_MARTS).
#' Data are in their raw form and should be cleaned either by hand or
#' using [cleanPacFIN].
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @template savedir
#' @template verbose
#' @importFrom magrittr %>%
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return An RData object is saved to the disk and the pulled data
#' are returned as a data frame.
#' The saved data can be read back in using [load], but note that
#' the object is named `bds.pacfin` inside of the `.RData` file, and thus,
#' the object will retain this name within your work space.
#' In February of 2021, the returned object was renamed from `out`
#' to `bds.pacfin` in an attempt to be more clear.
#'
#' Upon download, the data are changed from a long table to a wide table
#' using the combination of unique `FISH_ID` and `AGE_SEQUENCE_NUMBER`.
#' Users are warned if `verbose = TRUE` when there are non-unique `FISH_IDs` and these data
#' are automatically removed from the data frame.
#' This change from long to wide allows for rows equating to a single fish with columns
#' containing information about all measurements for that fish. Multiple age reads and
#' information about those reads such as age reader will be in the columns.
#' So, if your data had at most three duplicate reads,
#' then you will see numbers up to `3` pasted onto the end of relevant column names.
#' This is to help you match duplicate reads to other relevant information.
#' Not all double reads are currently available within PacFIN and
#' users should contact the ageing labs if they wish to inform ageing-error matrices.
#' 
#' `AGE_COUNT` is a somewhat cryptic column name and does not always make sense
#' when compared to `AGE_SEQUENCE_NUMBER`. It was determined that the former is
#' useful to identify how many potential agers were exposed to this fish.
#' For example, if `AGE_SEQUENCE_NUMBER` has a maximum value of three for a
#' given `FISH_ID`, then you can expect `AGE_COUNT` to be three for all three
#' rows in the PACFIN database for that fish. This is not always true though.
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
#' @seealso
#' * [cleanColumns] to change to legacy column names
#' * [cleanPacFIN] to manipulate and subset the returned object
#' @examples
#' \dontrun{
#' # Enter password in place of xxx below
#' mypw <- "xxx"
#' pd <- PullBDS.PacFIN(pacfin_species_code = "POP", password = mypw)
#' }
#'
PullBDS.PacFIN <- function(
  pacfin_species_code,
  username = getUserName("PacFIN"),
  password,
  savedir = getwd(),
  verbose = TRUE
  ) {

  #### Pull from PacFIN
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes and hit return\n")
  }
  rawdata <- getDB(sql.bds(pacfin_species_code),
    username = username, password = password)

  #### Check data and print warnings to the screen
  # Check if SAMPLE_AGENCY has values and remove
  sample_agency <- unique(rawdata[, "SAMPLE_AGENCY"])
  if (!is.na(sample_agency[1])) {
    warning("SAMPLE_AGENCY includes non-NULL values and should be extracted.\n",
      "Contact the package maintainer to request this and note the following:\n",
      "SAMPLE_AGENCY == ", knitr::combine_words(sample_agency))
  }
  rm(sample_agency)
  rawdata <- rawdata[, -match("SAMPLE_AGENCY", colnames(rawdata))]

  # Check OBSERVED_FREQUENCY > 1
  freq <- unique(rawdata[, "OBSERVED_FREQUENCY"])
  if (any(freq > 1)) {
    warning("Check OBSERVED_FREQUENCY b/c records are not unique to a single fish.")
  }
  rm(freq)

  # Check for NULL FISH_ID
  FISH_ID <- is.na(rawdata[["FISH_ID"]])
  if (sum(FISH_ID) > 0) {
    warning(call. = FALSE, immediate. = TRUE,
      "FISH_ID is NULL for ", sum(FISH_ID), " ", pacfin_species_code,
      " record(s) that was(were) removed (see below)."
      )
    print(rawdata[FISH_ID, c("SAMPLE_YEAR", "SAMPLE_NUMBER", "BDS_ID")])
    rawdata <- rawdata[!FISH_ID, ]
  }
  rm(FISH_ID)

  #### Manipulate rawdata columns
  subset <- !(duplicated(rawdata$FISH_ID) & is.na(rawdata$AGE_SEQUENCE_NUMBER))
  if (sum(!subset) > 0) {
    if (verbose) {
      message("Duplicated FISH_ID & AGE_SEQUENCE_NUMBER were removed.")
      print(rawdata[!subset, c("SAMPLE_YEAR", "SAMPLE_NUMBER")])
    }
    rawdata <- rawdata[subset, ]
  }

  # Fix FISH_LENGTH_TYPE_CODE if changed to a logical because only entry is fork length (F)
  # todo: check if this is needed now that R changed to stringsAsFactors = FALSE as the default
  rawdata[, "FISH_LENGTH_TYPE_CODE"] <- ifelse(
    rawdata[, "FISH_LENGTH_TYPE_CODE"] != FALSE,
    as.character(rawdata[, "FISH_LENGTH_TYPE_CODE"]),
    "F"
    )

  # Long to wide to facilitate estimating ageing error
  # Multiple BDS_IDs can pertain to a single FISH_ID, where each BDS_ID is an age read
  # todo: think about not changing column names
  # identical across rows: SAMPLE_ID, SAMPLE_NO, FISH_ID
  # unique across rows: BDS_ID, AGE_ID, AGE_SEQUENCE_NUMBER
  bds.pacfin <- rawdata %>%
    dplyr::rename(age = dplyr::matches("^AGE_IN_YEARS")) %>%
    dplyr::rename(agedby = dplyr::matches("PERSON_WHO_AGED")) %>%
    dplyr::rename(AGE_METHOD = dplyr::matches("AGE_METHOD_CODE")) %>%
    dplyr::mutate(AGE_SEQUENCE_NUMBER = tidyr::replace_na(.data[["AGE_SEQUENCE_NUMBER"]], 1)) %>%
    tidyr::pivot_wider(
      id_cols = !dplyr::matches("BDS_ID"),
      names_from = dplyr::matches("AGE_SEQUENCE_NUMBER"),
      values_from = c(dplyr::matches("AGE_ID"), AGE_METHOD:AGENCY_AGE_STRUCTURE_CODE),
      names_glue = "{.value}{AGE_SEQUENCE_NUMBER}",
      values_fill = NA)
  # Short check b/c pivot_wider can make lists
  if (!class(bds.pacfin[["age1"]]) %in% c("integer", "logical")) {
    stop("pivot_wider goofed up!")
  } else {
    bds.pacfin <- data.frame(bds.pacfin)
  }

  #### Save appropriate summaries
  savefn <- file.path(savedir,
    paste(sep = ".",
      "PacFIN", pacfin_species_code[1], "bds",
      format(Sys.Date(), "%d.%b.%Y"), "RData")
    )
  save(bds.pacfin, file = savefn)

  return(bds.pacfin)
}
