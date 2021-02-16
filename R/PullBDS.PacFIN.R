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
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return An RData object is saved to the disk and the pulled data
#' are returned as a data frame.
#' The saved data can be read back in using [load], but note that
#' the object is named inside of the `.RData` file, and thus,
#' the object will retain the name of `bds.pacfin` within your work space.
#' In February of 2021, the returned object was renamed from `out`
#' to `bds.pacfin` in an attempt to be more clear.
#'
#' Be aware that the data are changed from a long table to a wide table
#' using the combination of unique `FISH_IDs` in a given year and `AGE_SEQUENCE_NUMBER`.
#' Users are warned if there are non-unique `FISH_IDs` in a given year as these data
#' are automatically removed from the data frame. So, it is good to have `verbose = TRUE`.
#' This change from long to wide allows for rows equating to a single fish with columns
#' containing information about all measurements for that fish. Multiple age reads and
#' information about those reads such as age reader will be in the columns.
#'  So, if your data had at most three duplicate reads, then you will see numbers up
#' to `3` pasted onto the end of relevant column names. This is to help you match duplicate
#' reads to the relevant information. All information from age reader #1 will be in
#' a column with no tag, i.e., `age` instead of `age2`.
#'
#' The data frame is fairly raw with only one column,
#' `all_cluster_sum` -- the weight of all clusters sampled for a given sample number,
#' being calculated and added.
#' Some column names have been renamed, these pertain to the reading of ages.
#' For example `AGE_IN_YEARS` is now `age`, which can be followed by a number if the age
#' was a second, third, fourth, ... read. Numeric values signify columns like `age`
#' where the information was gleaned from another row with the same unique FISH_ID
#' in a given year. Having this information of double reads by FISH_ID will facilitate
#' the estimation of ageing error.
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
PullBDS.PacFIN <- function(pacfin_species_code,
  username = getUserName("PacFIN"), password, savedir = getwd(),
  verbose = TRUE) {

  #### Pull from PacFIN
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes and hit return\n")
  }
  rawdata <- getDB(sql.bds(pacfin_species_code),
    username = username, password = password)

  #### Check if SAMPLE_AGENCY has values and remove
  sample_agency <- unique(rawdata[, "SAMPLE_AGENCY"])
  if (!is.na(sample_agency[1])) {
    warning("SAMPLE_AGENCY includes non-NULL values and should be extracted.\n",
      "Contact the package maintainer to request this and note the following:\n",
      "SAMPLE_AGENCY == ", knitr::combine_words(sample_agency))
  }
  rm(sample_agency)
  rawdata <- rawdata[, -match("SAMPLE_AGENCY", colnames(rawdata))]

  #### Manipulate rawdata columns
  subset <- !(duplicated(rawdata$FISH_ID) & is.na(rawdata$AGE_SEQUENCE_NUMBER))
  if (sum(!subset) > 0 & verbose) {
    message("Duplicated FISH_ID & AGE_SEQUENCE_NUMBER were removed.")
    print(rawdata[!subset, c("SAMPLE_YEAR", "SAMPLE_NUMBER")])
  }

  # Fix FISH_LENGTH_TYPE if changed to a logical because only entry is fork length (F)
  rawdata[, "FISH_LENGTH_TYPE"] <- ifelse(
    rawdata[, "FISH_LENGTH_TYPE"] != FALSE,
    as.character(rawdata[, "FISH_LENGTH_TYPE"]),
    "F")

  bds.pacfin <- tidyr::pivot_wider(
    rawdata[subset, ] %>%
      dplyr::rename(age = dplyr::matches("^AGE_IN_YEARS")) %>%
      dplyr::rename(agedby = dplyr::matches("PERSON_WHO_AGED")) %>%
      dplyr::rename(AGE_METHOD = dplyr::matches("AGE_METHOD_CODE")) %>%
      mutate(AGE_SEQUENCE_NUMBER = tidyr::replace_na(AGE_SEQUENCE_NUMBER, 1)),
    names_from = AGE_SEQUENCE_NUMBER,
    values_from = dplyr::matches(
      match = "^age[dby]*$|^age_|_AGED|TURE_CODE|BDS_ID|AGE_ID|DATE_AGE|AGENCY_SAMPLE_NUMBER",
      ignore.case = TRUE),
    values_fill = NA,
    names_glue = "{.value}{AGE_SEQUENCE_NUMBER}") %>%
    dplyr::rename_with(.fn = ~gsub("1$", "", .x)) %>%
    data.frame

  bds.pacfin[, "all_cluster_sum"] <- stats::ave(FUN = sum, na.rm = TRUE,
    x = ifelse(yes = 0, no = bds.pacfin$CLUSTER_WEIGHT_LBS,
      test = duplicated(apply(MARGIN = 1, FUN = paste, collapse = " ",
        bds.pacfin[, c("SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER")]))),
    bds.pacfin$SAMPLE_NUMBER)

  #### Save appropriate summaries
  savefn <- file.path(savedir,
    paste(sep = ".",
      "PacFIN", pacfin_species_code[1], "bds",
      format(Sys.Date(), "%d.%b.%Y"), "RData")
    )
  save(bds.pacfin, file = savefn)

  return(bds.pacfin)
}
