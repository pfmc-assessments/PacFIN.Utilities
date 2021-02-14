#' Create a state column based on input column specified in `source`
#'
#' Create a categorical column that specifies which state each row is from.
#'
#' @details
#' With the creation of the comprehensive bds table in PacFIN, the column called
#' `SAMPLE_AGENCY` was deprecated; more specifically, the column is
#' available but filled with `NULL` values.
#' Thus, PacFIN.Utilities no longer maintains `SAMPLE_AGENCY` that was being converted
#' to `SOURCE_AGID` and all identification of
#' state records should be based on `AGENCY_CODE` or `SOURCE_AGID`, where the latter is
#' just the converted column name (see [cleanColumns]).
#' `AGENCY_CODE` is a column created by PacFIN to identify which agency provided
#' the data. In the 2019 sablefish data there were four unique values in `AGENCY_CODE`,
#' * C - CDFW
#' * O - ODFW
#' * W - WDFW
#' * M - `SAMPLE_AGENCY == NMFS Tiburon` samples from 1997.
#' None of the `M` samples were in the comprehensive table as of 2021.
#'
#' It is no longer advisable as of February 14, 2021 to create states based on
#' `PSMFC_CATCH_AREA_CODE` or `PSMFC_ARID` because areas are not mutually
#' exclusive to a state. Previous code set areas 3[a-z] to Washington,
#' 2[a-z] to Oregon, and 3[a-z] to California.
#' The [PacFIN documentation](https://pacfin.psmfc.org/wp-content/uploads/2019/03/PacFIN_Comprehensive_BDS_Commercial.pdf)
#' suggests that the following area codes can be assigned to the following states:
#' * WA: 1C, 2A, 2B, 2C, 2E, 2F, 3A, 3B, 3C, 3N, 3S
#' * OR: 1C, 2A, 2B, 2C, 2E, 2F, 3A, 3B, CS
#' * CA: 1A, 1B, 1C
#' Rather than supporting one or the other, users are now left to deciphering which
#' PSMFC areas they want to assign to which states on there own. Hopefully, the guidance
#' above will be helpful. If you see the need to use PSMFC_CATCH_AREA_CODE to set states
#' please contact the package maintainer.
#'
#' @export
#' @seealso [cleanPacFIN] calls `getState`.
#'
#' @template Pdata
#' @param source The column name where state information is located in
#' \code{Pdata}. See the function call for options, where only the first
#' value will be used.
#' @template verbose
#'
#' @return The input data frame is returned with an additional column,
#' `state`, which is filled with two-character values identifying the state or
#' a three-character value `UNK` for all rows that do not have an assigned state.
#' All rows are returned, but users should pay attention to the warning that is
#' returned for rows that have no state id.
#'
#' @examples
#' data(XMPL.BDS)
#' invisible(getState(XMPL.BDS, verbose = FALSE))
#' testthat::expect_equivalent(
#'   table(getState(XMPL.BDS, verbose = FALSE)[, "state"])[3],
#'   17292)
#'
getState <- function (Pdata,
  source = c("AGENCY_CODE", "SOURCE_AGID"),
  verbose = FALSE) {

  if (any(source %in% c("PSMFC_CATCH_AREA_CODE", "PSMFC_ARID"))) {
    stop("'PSMFC_CATCH_AREA_CODE' and 'PSMFC_ARID' are no longer supported ",
      "inputs to getState(source = ).")
  }
  source <- match.arg(source, several.ok = FALSE)
  colid <- match(source, colnames(Pdata))
  if (is.na(colid[1])) {
    stop("The column '", source, "' was not found in Pdata.")
  }

  Pdata$state <- as.character(Pdata[, source])

  Pdata[, "state"] <- vapply(Pdata[, "state"], FUN = switch,
    FUN.VALUE = "character",
    C = "CA", CalCOM = "CA", CALCOM = "CA",
    O = "OR",
    W = "WA",
    "UNK")

  states <- c("OR", "CA", "WA")

  nostate <- sum(!Pdata[, "state"] %in% states)

  if (verbose) {
    message("There are ", nostate,
      " records for which the state (i.e., 'CA', 'OR', 'WA')",
      "\ncould not be assigned and were labeled as 'UNK'.")
    write.table(table(Pdata[, "state"]),
      col.names = FALSE, row.names = FALSE)
  } # End if verbose

  return(Pdata)

}
