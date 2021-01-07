#' Create a state column based on input column specified in `source`
#' 
#' Create a categorical column that specifies which state each row is from.
#' 
#' @export
#' @seealso [cleanPacFIN] calls this function
#'
#' @template Pdata
#' @param source The column name where state information is located in
#' \code{Pdata}. See the function call for options, where only the first
#' value will be used.
#' @template verbose
#' @examples
#' data(XMPL.BDS)
#' invisible(getState(XMPL.BDS, verbose = FALSE))
#' testthat::expect_equivalent(
#'   table(getState(XMPL.BDS, verbose = FALSE)[, "state"])[3],
#'   17292)
#'
getState <- function (Pdata,
  source = c("SOURCE_AGID", "PSMFC_ARID", "SAMPLE_AGENCY", "AGID", "AGENCY_CODE"),
  verbose = FALSE) {

  source <- match.arg(source, several.ok = FALSE)

  Pdata$state <- as.character(Pdata[, source])

  if ( source == "PSMFC_ARID" ) {
    Pdata$state[Pdata$state %in% c("3A","3B","3S")] = "WA"
    Pdata$state[Pdata$state %in% c("2A","2B","2C")] = "OR"
    Pdata$state[Pdata$state %in% c("1A","1B","1C","CAL")] = "CA"
  }

  Pdata[, "state"] <- vapply(Pdata[, "state"], FUN = switch,
    FUN.VALUE = "character",
    C = "CA", CalCOM = "CA", CALCOM = "CA",
    O = "OR",
    W = "WA",
    "UNK")

  Pdata$state[Pdata$SOURCE_AGID == "CalCOM"] <- "CA"
  Pdata$state[Pdata$SOURCE_AGID == "CALCOM"] <- "CA"

  # Remove stateless data

  states <- c("OR", "CA", "WA")

  nostate <- sum(!Pdata[, "state"] %in% states)

  if (verbose) {
    message("There are ", nostate,
      " records for which the state (i.e., 'CA', 'OR', 'WA')",
      "\ncould be assigned and were labeled as 'UNK'.")
    write.table(table(Pdata[, "state"]),
      col.names = FALSE, row.names = FALSE)
  } # End if verbose

  return(Pdata)

}
