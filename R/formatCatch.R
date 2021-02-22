#' Format catches from long to wide
#' 
#' Transform a long data frame of catches to a wide data frame
#' using [tidyr::pivot_wider].
#' The column names of the wide data frame will be in the format
#' needed for the stratification of stage-2 expansions of
#' composition data, i.e., [getExpansion_2].
#' 
#' @param catch A data frame with at least a column specifying
#' the year the catches took place
#' (e.g., LANDING_YEAR is the column name upon download from PacFIN),
#' a column for variable(s) specified in strat, and
#' a column that holds the measured catches named 
#' (e.g., ROUND_WEIGHT_LBS is the column name upon download from PacFIN).
#' @template strat
#' @param yearname A character string used to search for year in `catch`.
#' Ensure that the string is unique enough that it will only return one value.
#' Though, if you would like catch by year and month you can pass a single
#' character value that includes both of these, for example `_YEAR|_MONTH`.
#' This is underhanded and not what the original functionality of this function
#' intended, but feel free to use it.
#' This is not case specific.
#' @param valuename The column name that contains the
#' catch values in the data frame `ROUND_WEIGHT_LBS` is the default and the
#' name of the column that contains landed weight converted to round weight
#' in lbs. The conversion factor from landed weight to round weight is
#' weight_of_catch * NVL(conversion_factor,1) and 1 mt equals 2,204.62 lbs.
#' This is not case specific.
#'
#' @export
#' @author Kelli F. Johnson
#' @return A data frame in wide format with `yearname` being the first column
#' ordered from oldest to newest. Stratification columns follow and the names
#' are separated with a full stop when more than one stratification is provided.
#' Values in the rows will be the `valuename` summed across all available values.
#' If there are no relevant values for a given stratification-year combination,
#' then the entry will be zero.
#'
formatCatch <- function(catch, strat,
  yearname = "^Year|^Yr|Landing_Y|Sample_Y",
  valuename = "ROUND_WEIGHT_LBS") {

  if ("state" %in% strat & !"state" %in% colnames(catch)) {
    catch <- getState(catch, verbose = FALSE)
  }
  if ("geargroup" %in% strat & !"geargroup" %in% colnames(catch)) {
    catch <- getGearGroup(catch, verbose = FALSE)
  }

  # Reshape the data into wide format and replace NA with zeros
  out <- catch %>% tidyr::pivot_wider(
    values_fn = sum, values_fill = 0,
    id_cols = dplyr::matches(match = yearname, ignore.case = TRUE),
    names_from = dplyr::matches(strat), names_sep = ".",
    values_from = dplyr::matches(match = valuename, ignore.case = TRUE)
    ) %>% data.frame()

  return(out)

}
