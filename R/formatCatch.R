#' Format catches from long to wide
#' 
#' Transform a long data frame of catches to a wide data frame
#' using [stats::reshape].
#' The column names of the wide data frame will be in the format
#' needed for the stratification of stage-2 expansions of
#' composition data, i.e., [getExpansion_2].
#' 
#' @param catch A data frame with at least a column specifying
#' the year the catches took place (e.g., Year), a column for
#' variable(s) specified in strat, and a column that holds
#' the measured catches named (i.e., `catch[, valuename]`).
#' @template strat
#' @param valuename The column name that contains the
#' catch values in the data frame `catch`.
#' 
formatCatch <- function(catch, strat, valuename = "catch") {

  # Get the column for idvar
  yearn <- grep("^Year|^Yr", colnames(catch),
    ignore.case = TRUE, value = TRUE)

  # Get the column to be used as values in the wide format
  colnames(catch) <- gsub(valuename, "catch", colnames(catch))

  # Get a new column that will lead to column names in the wide format
  # and check for duplicates per grouping
  if ("state" %in% strat & !"state" %in% colnames(catch)) {
    catch <- getState(catch, source = "AGID", verbose = FALSE)
  }
  if ("geargroup" %in% strat & !"geargroup" %in% colnames(catch)) {
    catch <- getGearGroup(catch, verbose = FALSE)
  }
  out <- catch[, c(yearn, strat, "catch")]
  out$merged <- apply(out[, strat, drop = FALSE], 1, paste, collapse = ".")
  if (any(duplicated(paste(out[, "merged"], out[, yearn])))) {
    out <- stats::aggregate(out[, "catch", drop = FALSE],
      out[, c(yearn, "merged"), drop = FALSE], FUN = sum, na.rm = TRUE)
  }

  # Reshape the data into wide format and replace NA with zeros
  out <- stats::reshape(out, direction = "wide",
    timevar = "merged", idvar = yearn)
  out <- out[, grep(paste0("catch|", yearn), colnames(out))]
  out[is.na(out)] <- 0
  colnames(out) <- gsub("catch\\.", "", colnames(out))

  return(out)

}
