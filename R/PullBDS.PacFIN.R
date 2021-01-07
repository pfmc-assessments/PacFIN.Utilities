#' Pull biological data from PacFIN database
#'
#' Pull biological data from the Comprehensive Biological Data Table
#' in the PacFIN database.
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @template savedir
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return RData frame is saved to the disk and the pulled data
#' are returned as a data frame.
#' The data frame is fairly raw in its form with only one column
#' added, `all_cluster_sum`, which is the weight of all clusters sampled
#' for a given sample number. Typically, [cleanPacFIN] will be used
#' on the data frame after its retrieval.
#' @seealso [cleanPacFIN] to manipulate and subset the returned object.
#' @examples
#' \dontrun{
#' # Enter password in place of xxx below
#' mypw <- "xxx"
#' pd <- PullBDS.PacFIN(pacfin_species_code = "POP", password = mypw)
#' }
#'
PullBDS.PacFIN <- function(pacfin_species_code,
  username = getUserName("PacFIN"), password, savedir = getwd()) {

  #### Pull from PacFIN
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes\n")
  }
  data <- getDB(sql.bds(pacfin_species_code),
    username = username, password = password)

  #### Manipulate data columns
  subset <- !(duplicated(data$FISH_ID) & is.na(data$AGE_SEQUENCE_NUMBER))
  if (sum(!subset) > 0) {
    message("Duplicated FISH_ID & AGE_SEQUENCE_NUMBER were removed.")
    print(data[!subset, c("SAMPLE_YEAR", "SAMPLE_NUMBER")])
  }
  data <- data[subset, ]
  data[, "AGE_SEQUENCE_NUMBER"] <- ifelse(is.na(data[, "AGE_SEQUENCE_NUMBER"]),
    1, data[, "AGE_SEQUENCE_NUMBER"])
  out <- stats::reshape(data, direction = "wide",
    timevar = "AGE_SEQUENCE_NUMBER",
    idvar = c("SAMPLE_YEAR", "FISH_ID")
    )
  colnames(out) <- gsub("^AGE_IN_YEARS\\.", "age", colnames(out))
  colnames(out) <- gsub("PERSON_WHO_AGED\\.", "agedby", colnames(out))
  colnames(out) <- gsub("AGE_METHOD_CODE\\.", "AGE_METHOD", colnames(out))
  colnames(out) <- gsub("\\.1", "", colnames(out))
  # Find all columns that end with letters or letters then number w/o '.'
  out <- out[, grep("[a-zA-Z]$|[a-zD][0-9]+$", colnames(out))]
  # Order columns with the transform to wide columns last
  out <- out[, c(grep("[A-Z]$",colnames(out)), grep("[0-9]$", colnames(out)))]

  out[, "all_cluster_sum"] <- stats::ave(FUN = sum, na.rm = TRUE,
    x = ifelse(yes = 0, no = out$CLUSTER_WEIGHT_LBS,
      test = duplicated(apply(MARGIN = 1, FUN = paste, collapse = " ",
        out[, c("SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER")]))),
    out$SAMPLE_NUMBER)

  #### Save appropriate summaries
  savefn <- file.path(savedir, paste(sep = ".",
    "PacFIN", pacfin_species_code[1], "bds",
    format(Sys.Date(), "%d.%b.%Y"),
    "RData"))
  save(out, file = savefn)

  return(out)
}
