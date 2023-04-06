#' Get age information for each fish
#'
#' Get age information for each fish, or row of the data.
#' This functionality was originally in cleanAges, moved to
#' cleanPacFIN, and is now compartmentalized here, but called in
#' [cleanPacFIN] and stored in the `Age` column.
#'
#' @details
#' Age types listed in keep (or derivatives of those) are used to filter
#' for ages that should be kept, i.e., not returned as `NA`.
#' If `FISH_AGE_YEARS_FINAL` doesn't have an entry, where this should be the
#' best age available, then the age reads in `age1`, `age2`, ... are
#' averaged over and rounded to the next integer
#' (i.e., [ceiling]) if their methods are acceptable.
#' When `FISH_AGE_YEARS_FINAL` does have an entry, this age is kept if one of
#' the methods in `AGE_METHOD` is listed in `keep`. It is currently unclear
#' how `FISH_AGE_YEARS_FINAL` is determined because it doesn't always equal
#' the value from the first age reader. Sometimes, it is `ceiling(mean())`,
#' other times it appears to default to break and burn, and other random combos.
#' We use the best judgment of those who created the column that they know what
#' they are doing, but please feel free to report errors in PacFIN to the
#' github issues or the package maintainer after contacting the state representative.
#'
#' The input argument of `keep` can contain a mix of numeric and character values,
#' but all values will be converted to character values. See below for conversions,
#' where it is easiest to just supply a vector of values shown on the left before
#' the double equal sign because all values in your data set to the right will be
#' converted prior to searching for those in `keep`.
#' For example, `keep = c("B", "L")` will keep all `"BB"`, `1`, and `6` entries as well.
#' * "B" == 1, "BB", or break and burn
#' * "S" == 2, "SR" (California), or surface read
#' * 3 == scales (Oregon data)
#' * "T" == 4, "TS" (California), "X" (Washington), or thin section
#' * "O" == 5 or optical scanner
#' * "L" == 6 or age derived from length
#' * 9 == unable to read or age
#' * U == unknown
#'
#' The final age assigned to a fish when there are multiple age reads depends on the
#' ageing lab. See issue #49 for a semi-detailed discussion. Sometimes, reconciling
#' happens where the best age will not be equal to any age read from a specific reader
#' but the readers get together to determine the best read.
#' Washington uses the following order of operations to assign the best age given
#' there are multiple reads. Note, that they will never report more than three reads
#' because that is the limitation of their database.
#' * age1 if only one read;
#' * randomly chosen age between age-read 1 and age-read 2 if the difference between
#' the two reads is less than three;
#' * if the difference between two reads is more than three, then the best age is
#' manually keyed in from that designated by a human;
#' * if three reads are equal, then the value is automatically assigned; and
#' * if there are three reads and they are not equal then an age reader must choose
#' the final value.
#'
#' @template Pdata
#' @template verbose
#' @template keep
#' @param col.bestage A character value noting the column name that contains the best age
#' to use going forward for a given fish. Over time, this has been
#' `FISH_AGE_YEARS_FINAL` and `FINAL_FISH_AGE_IN_YEARS` for PacFIN.
#'
#' @export
#' @return A vector of ages in years with a length the same as the number
#' of rows present in the input data frame `Pdata`.
#' If all ages are `NA`, then a vector of `NA` values is quickly returned.
#'
getAge <- function(Pdata,
                   verbose = FALSE,
                   keep,
                   col.bestage = "FISH_AGE_YEARS_FINAL") {
  if (!col.bestage %in% colnames(Pdata)) {
    Pdata[[col.bestage]] <- NA
  }
  if (length(grep("^age[0-9]*$", colnames(Pdata))) == 0) {
    Pdata[["age"]] <- if (any(grepl("AGE_IN_YEARS", colnames(Pdata)))) {
      Pdata[["AGE_IN_YEARS"]]
    } else {
      NA
    }
  }

  #### Return if all ages are NA
  searchfor <- paste(col.bestage, "^age[0-9]*$", sep = "|")
  if (all(is.na(Pdata[, grep(searchfor, colnames(Pdata))]))) {
    return(rep(NA, length = NROW(Pdata)))
  }

  # Select relevant columns because this function only returns a vector not a data frame
  # so we don't need to keep all the columns, and it makes it hard to debug when there
  # are so many columns in the data.
  Pdata <- Pdata %>% dplyr::select(dplyr::matches("AGE|FISH_ID", ignore.case = TRUE))

  keep <- gsub("1", "B", keep)
  keep <- gsub("BB", "B", keep)
  keep <- gsub("2", "S", keep)
  keep <- gsub("^SR$", "S", keep)
  keep <- gsub("4", "T", keep)
  keep <- gsub("^TS$", "T", keep)
  keep <- gsub("X", "T", keep)
  keep <- gsub("5", "O", keep)
  keep <- gsub("6", "L", keep)
  keep <- unique(keep)

  # Standardize all columns that have AGE_METHOD, AGE_METHOD2, ...
  if (
    inherits(Pdata[["FINAL_FISH_AGE_CODE"]], "character") |
      inherits(Pdata[["FISH_AGE_CODE_FINAL"]], "character")
  ) {
    message(
      "PacFIN was fixed and now supplies FINAL_FISH_AGE_CODE or ",
      "FISH_AGE_CODE_FINAL,\n and thus, the code in `getAge()` should be",
      " updated to reflect this; look for todo."
    )
  }
  Pdata <- Pdata %>%
    dplyr::mutate(dplyr::across(
      # dplyr::matches("AGE_CODE|AGE_M"),
      # todo: uncomment above when FINAL_FISH_AGE_CODE has correct
      # codes rather than duplicates of the final age
      dplyr::matches("AGE_M"),
      ~ dplyr::case_when(
        .x %in% c("B", "BB", 1) ~ "B",
        .x %in% c("S", "SR", 2) ~ "S",
        .x %in% c("T", "TS", "X", 4) ~ "T",
        .x %in% c("O", 5) ~ "O",
        .x %in% c("L", 6) ~ "L",
        TRUE ~ as.character(.x)
      )
    ))

  # Only keep ages in age column if it is an approved method
  for (iiname in grep("AGE_METHOD[0-9]*$", colnames(Pdata), value = TRUE)) {
    iinum <- gsub("[a-zA-Z_]", "", iiname)
    Pdata[, paste0("age", iinum)] <- ifelse(
      test = Pdata[, paste0("AGE_METHOD", iinum)] %in% keep,
      yes = Pdata[, paste0("age", iinum)],
      no = NA
    )
  }

  # Average all non-NA ages available if col.bestage is NA
  # Not sure why this is happening in PacFIN
  # 1 California sablefish
  # 8300 WA records of various spp emailed Theresa on 2021-03-06
  Age <- ifelse(
    test = (
      # todo
      # (Pdata[["FISH_AGE_CODE_FINAL"]] %in% keep) |
      # (Pdata[["FINAL_FISH_AGE_CODE"]] %in% keep) |
      !is.na(Pdata[[col.bestage]])
    ),
    yes = Pdata[[col.bestage]],
    no = apply(Pdata[, grep("age[0-9]*$", colnames(Pdata)), drop = FALSE], 1,
      FUN = function(x) {
        if (all(is.na(x))) {
          return(NA)
        }
        return(ceiling(mean(stats::na.omit(x), na.rm = FALSE)))
      }
    )
  )

  # Remove Age if none of the available age methods are in keep
  Age[
    apply(
      X = Pdata[, grep("AGE_METHOD[0-9]*$", colnames(Pdata)), drop = FALSE],
      MARGIN = 1,
      FUN = function(x) !any(x %in% keep)
    )
  ] <- NA

  if (verbose) {
    message(
      "\nAges ranged from ", min(Age, na.rm = TRUE), " to ",
      max(Age, na.rm = TRUE), " (years)."
    )
    print(summaryAgeMethod(Pdata, verbose = TRUE))
    badages <- Pdata[!is.na(Pdata[[col.bestage]]), ]
    agematch <- sapply(1:NROW(badages), FUN = function(x) {
      match(
        badages[x, col.bestage],
        badages[x, grep("age[0-9]*$", colnames(badages))]
      )
    })
    message(
      "\n", col.bestage, " matches value(s) for agers 1, 2, ...;",
      " NA means no exact match."
    )
    print(table(agematch, useNA = "always"))
    badages <- badages[which(is.na(agematch)), ]
    badages[["mean"]] <- ceiling(
      apply(badages[, grep("age[0-9]*$", colnames(badages)), drop = FALSE],
        MARGIN = 1,
        mean, na.rm = TRUE
      )
    )
    if (length(which(badages[[col.bestage]] != badages[["mean"]])) > 0) {
      message(
        "Investigate the following SAMPLES for errors because\n",
        col.bestage, " doesn't match the mean of all reads:"
      )
      print(badages[
        which(badages[[col.bestage]] != badages[["mean"]]),
        grep("FISH_ID|FINAL|age", colnames(badages))
      ])
    }
  }

  return(Age)
}

#' Summary of available age methods
#'
#' Use [dplyr::count()] to summarize the available age types.
#'
#' @template Pdata
#' @template verbose
#'
#' @return A data frame of counts for each age method.
#' If `verbose`, then text explaining the summary is also printed to the
#' screen prior to returning the table.
#'
summaryAgeMethod <- function(Pdata,
                             verbose = TRUE) {
  if (verbose) {
    message(
      "The table below includes the number of samples (n) per ageing method.\n",
      "Each age sequence number, the PacFIN delineation for age read, has its",
      " own column.\nThe number of columns depends on the maximum number of",
      " age reads available.\nIf ANY method for a given fish matches those",
      " in keep, then that age will be kept\neven if PacFIN did not use the",
      " age to create the final fish age."
    )
  }
  Pdata %>%
    dplyr::select(dplyr::matches("AGE_METHOD")) %>%
    dplyr::count(dplyr::across(dplyr::everything()))
}
