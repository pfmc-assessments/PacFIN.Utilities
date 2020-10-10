#' Pull Catch Data from PacFIN Database
#'
#' Pull Catch Data from the Comprehensive Fish Ticket table
#' in the PacFIN database.
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @template savedir
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return RData frames is saved to the disk and the pulled data
#' are returned as a data frame.
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
  data <- data[!(duplicated(data$FISH_ID) & is.na(data$AGE_SEQUENCE_NUMBER)), ]
  data <- data[!(duplicated(data$FISH_ID) & data$AGE_SEQUENCE_NUMBER) == 1, ]
  data[, "AGE_SEQUENCE_NUMBER"] <- ifelse(is.na(data[, "AGE_SEQUENCE_NUMBER"]),
    1, data[, "AGE_SEQUENCE_NUMBER"])
  out <- stats::reshape(data, direction = "wide",
    timevar = "AGE_SEQUENCE_NUMBER",
    idvar = c("SAMPLE_YEAR", "FISH_ID")
    )
  colnames(out) <- gsub("^AGE_IN_YEARS\\.", "age", colnames(out))
  colnames(out) <- gsub("PERSON_WHO_AGED\\.", "agedby", colnames(out))
  colnames(out) <- gsub("\\.1", "", colnames(out))
  out <- out[, grep("[a-zA-Z]$|[a-z][0-9]+$", colnames(out))]
  out <- out[, c(grep("[A-Z]$",colnames(out)), grep("[0-9]$", colnames(out)))]

  out[,"all_cluster_sum"] <- stats::ave(FUN = sum, na.rm = TRUE,
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
