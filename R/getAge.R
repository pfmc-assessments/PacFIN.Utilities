#' Get age information for each fish
#'
#' Get age information for each fish, or row of the data.
#' This functionality was originally in cleanAges, moved to
#' cleanPacFIN, and is now compartmentalized here, but called in
#' [cleanPacFIN].
#'
#' @details
#' Age types listed in keep (or derivatives of those) are used to filter
#' for ages that should be kept, i.e., not returned as `NA`.
#' If `FISH_AGE_YEARS_FINAL` doesn't have an entry, where this should be the
#' best age available, then the age reads in `age`, `age2`, ... are
#' averaged over, if their method is acceptable, to create an entry for
#' `FISH_AGE_YEARS_FINAL`. These best ages are then returned for those
#' types that are included in `keep`.
#'
#' The input argument of `keep` can contain a mix of numeric and character values,
#' but all values will be converted to character values. For example,
#' * 1 == "B", "BB", or break and burn
#' * 2 == "S" or surface read
#' * 3 == scales (Oregon data)
#' * 4 == "T" or thin section
#' * 5 == "O" or optical scanner
#' * 6 == age derived from length (Oregon data)
#' * 9 == unable to read or age
#' * U == unknown
#'
#' @template Pdata
#' @template verbose
#' @template keep
#' @importFrom magrittr %>%
#'
#' @export
#' @return A vector of ages.
#'
getAge <- function(Pdata, verbose, keep){
  if (!"FISH_AGE_YEARS_FINAL" %in% colnames(Pdata)) {
    Pdata$FISH_AGE_YEARS_FINAL <- NA
  }

  keep <- gsub("1", "B", keep)
  keep <- gsub("BB", "B", keep)
  keep <- gsub("2", "S", keep)
  keep <- gsub("4", "T", keep)
  keep <- gsub("5", "O", keep)
  keep <- gsub("6", "L", keep)

  Pdata <- Pdata %>%
  dplyr::mutate(dplyr::across(
    dplyr::starts_with("AGE_METHOD"),
    ~ dplyr::case_when(
      .x %in% c("B", "BB", 1) ~ "B",
      .x %in% c("S", 2) ~ "S",
      .x %in% c("T", 4) ~ "T",
      .x %in% c("O", 5) ~ "O",
      .x %in% c("L", 6) ~ "L",
      TRUE ~ .x)))

  for (iiname in grep("AGE_METHOD[0-9]+$", colnames(Pdata), value = TRUE)) {
    iinum <- type.convert(as.is = TRUE, gsub("[a-zA-Z_]", "", iiname))
    Pdata[, paste0("age", iinum)] <- ifelse(
      Pdata[, paste0("AGE_METHOD", iinum)] %in% keep,
      Pdata[, paste0("age", iinum)],
      NA)
  }

  # Take the average of all non-zero or non-NA ages if multiple reads
  # are available
   age <- ifelse(!is.na(Pdata$FISH_AGE_YEARS_FINAL),
    Pdata$FISH_AGE_YEARS_FINAL,
    apply(Pdata[, grep("age[0-9]+$", colnames(Pdata))], 1,
      FUN = function(x) {
        if (all(is.na(x))) return(NA)
        if (all(x == 0)) return(NA)
        return(mean(x[!x %in% c(0, NA)], na.rm = FALSE))
    })
    )
  # Remove the age if none of the available age methods are in keep
  age[
    apply(
      Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata)), drop = FALSE],
      1, FUN = function(x) !any(x %in% keep)
      )
    ] <- NA

  if (verbose) {
    message("\nThe following age types were kept in the data:")
    message(knitr::combine_words(unique(unlist(
        Pdata[, grep("AGE_METHOD", colnames(Pdata))]
        )))
      )
    message("Ages (years) ranged from ", min(age, na.rm = TRUE), " to ",
      max(age, na.rm = TRUE))
  }

  return(age)
}