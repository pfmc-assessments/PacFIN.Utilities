#' Aggregate length, age, or age-at-length composition data by strata
#'
#' `getComps()` first sets up the local environment, then runs
#' `getcomps_long()`, and finally returns the results of `getcomps_long()`,
#' which calculates summaries by groupings. `getcomps_long()` was developed to
#' make use of {tidyverse} but it still produces all of the old output. In the
#' future, this function will be replaced by code that can summarize both
#' survey data or commercial data. Until then, this is the best we can do and
#' it mimics old output such that legacy code will not break. You can and
#' should pass arguments to `getcomps_long()` using pass through arguments in
#' your call to `getComps()`, i.e., `...`, especially when calculating
#' composition data related to age. See the input arguments for more details.
#'
#' @details
#' This function uses the expansions created in [getExpansion_1()] and
#' [getExpansion_2()] using `"weightid"`. Thus, whatever column name is passed
#' to `weightid` will be used as the weight for each group. See the
#' documentation for the pass-through arguments, i.e., `...`
#'
#' @inheritParams tableSample
#' @param Comps The type of composition data to create. See the function call
#'   for the available options. The first option listed is the default, which
#'   creates conditional age-at-length compositions by adding both `lengthcm`
#'   and `Age` to the grouping structure.
#' @param defaults The default stratification columns which will typically be
#'   left at their default value of `c("fleet", "fishyr", "season")`.
#' @param ... Pass additional arguments to `getcomps_long()`, such as
#'   `dropmissing = FALSE` where the default behavior is `dropmissing = TRUE`.
#'   The most important argument to consider modifying is `getComps(weightid =
#'   "Final_Sample_Size_L")`, which will only be applicable to length data.
#'   Instead of the default, you might want to weight each stratification by
#'    the final sample sizes found for age data, i.e., `getComps(weightid =
#'   "Final_Sample_Size_A")`. You can pass `weightid` any column that exists in
#'   `Pdata`. Legacy code required you to set `Final_Sample_Size` outside of a
#'   function call with something like `Pdata$Final_Sample_Size =
#'   Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2` and all weighting was
#'   done on the column called `"Final_Sample_Size"` but this is no longer the
#'   case.
#' @return
#' A long data frame of weights for each grouping structure. Columns
#' identifying the grouping structures will come first, followed by columns
#' with sample sizes per group by sex category. The documentation for these
#' sample size columns is sparse because this function is set to be deprecated
#' next cycle and replaced with a simplified path to writing composition data.
#' But, information is present for males (sometimes abbreviated with an m),
#' females (sometimes abbreviated with an f), unsexed (sometimes abbreviated
#' with a u), and sexed (noted as both or b) records. If no sex is provided
#' then it is assumed all are unsexed and will be returned as such.
#' @export
#' @author Andi Stephens, Kelli F. Johnson
#' @seealso
#' * [getExpansion_2()] should be run before this function.
#' * [writeComps()] can be run on the output from this function.
#'
getComps <- function(Pdata,
                     strat = NULL,
                     Comps = c("LEN", "AGE", "AAL"),
                     defaults = c("fleet", "fishyr", "season"),
                     ...) {
  # Check for expansion factor
  if (length(Pdata$Final_Sample_Size) == 0) {
    stop(paste(
      "\ngetComps relies on the column labeled 'Final_Sample_Size'\n",
      "please make sure this column (the expansion factor) has a value.\n\n",
      "Example: Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1",
      "* Pdata$Expansion_Factor_2\n"
    ))
  } # End if

  # Set up stratification

  usualSuspects <- defaults

  # Avoid duplication
  strat <- strat[!strat %in% usualSuspects]
  Comps <- match.arg(Comps)
  TowStrat <- c(
    strat,
    switch(Comps,
      LEN = usualSuspects,
      AGE = usualSuspects,
      c(usualSuspects, "lengthcm", "Age")
    )
  )
  usualSuspects <- switch(
    Comps,
    LEN = c(usualSuspects, "lengthcm"),
    AGE = c(usualSuspects, "Age"),
    c(usualSuspects, "lengthcm", "Age")
  )

  ageComps <- getcomps_long(
    data = Pdata,
    towstrat = TowStrat,
    type = utils::tail(usualSuspects, 1),
    ...
  )
  invisible(ageComps)
}

#' Create a long data frame to prepare compositions for [writeComps()]
#' @param data A data frame. The data frame must have the column names that
#'   are specified for the next four input arguments, `towstrat`, `type`,
#'   `towid`, and `weightid`.
#' @param towstrat A vector of character values providing the column names for
#'   which you want compositions for.
#' @param type A character value specifying which category to summarize by,
#'   i.e., `"length"` or `"Age"`.
#' @param towid A vector of character values providing the column names that
#'   generate a unique id for each sample.
#' @param weightid A character value giving the column name that holds the
#'   value to be summed for each type and strata.
#' @param dropmissing A logical value supplied to the `drop` argument in
#'   [stats::aggregate()] that specifies whether or not to keep all levels in
#'   the data even if there are no values to report for summaries.
#' @export
#' @describeIn getComps The actual workhorse behind `getComps()`.
#'
getcomps_long <- function(data,
                          towstrat,
                          type,
                          towid = "SAMPLE_NO",
                          weightid = "Final_Sample_Size_L",
                          dropmissing = TRUE) {
  if (!all(towstrat %in% colnames(data))) {
    stop("Not all towstrat are available.")
  }
  if (!type %in% colnames(data)) {
    stop(
      "'type' must be a column in data",
      " i.e., 'lengthcm' or 'Age'"
    )
  }

  # Create a unique id for each sample
  data[, "uniqueid"] <- apply(data[, towid, drop = FALSE],
    1, paste,
    collapse = "_"
  )
  towid <- "uniqueid"
  # Find which column contains the sex data or create unsexed
  sexn <- grep("sex", colnames(data), ignore.case = TRUE, value = TRUE)
  if (length(sexn) > 1) {
    warnings(
      "Multiple columns match 'sex' (ignoring case), taking the first one"
    )
    sexn <- sexn[1]
  }
  if (length(sexn) == 0) {
    sexn <- "SEX"
    data[, sexn] <- "U"
    warning("SEX was missing from the data and set to 'U' for unsexed fish")
  }
  if (is.character(data[, sexn])) {
    data$sexed <- data[, sexn]
    data$sexed[data$sexed %in% c("M", "F")] <- "B"
    data[, sexn] <- factor(data[, sexn], levels = c("F", "M", "U"))
    data$sexed <- factor(data[, "sexed"], levels = "B")
  }

  # FREQ... stores the number of fish that sum to the weightid
  freqn <- grep("freq", colnames(data), ignore.case = TRUE, value = TRUE)
  if (length(freqn) == 0) stop("FREQ is missing from the data.")

  tstratwsex <- c(towstrat, sexn)
  Cstrat <- c(towstrat, type)
  cstratwsex <- c(Cstrat, sexn)

  tstratw_b <- c(towstrat, "sexed")
  cstratw_b <- c(Cstrat, "sexed")

  # Find which samples only have unsexed fish
  data[, "Uonly"] <- getunsexedsamps(data[, towid], data[, sexn])
  data[, "Bonly"] <- getunsexedsamps(data[, towid], data[, "sexed"], good = "B")

  comp <- merge(
    by = tstratwsex, all = TRUE,
    stats::aggregate(
      data[, c(weightid, freqn)],
      by = data[, cstratwsex, drop = FALSE],
      sum, na.rm = TRUE, drop = dropmissing
    ),
    stats::aggregate(
      list("tows" = data[, towid], "ONLY_U_TOWS" = data[, c("Uonly")]),
      by = data[, tstratwsex, drop = FALSE],
      lenique, drop = dropmissing
    )
  )

  comp_2 <- merge(
    by = tstratw_b, all = TRUE,
    stats::aggregate(
      data[, c(weightid, freqn)],
      by = data[, cstratw_b, drop = FALSE],
      sum, na.rm = TRUE, drop = dropmissing
    ),
    stats::aggregate(
      list("btows" = data[, towid], "ONLY_B_TOWS" = data[, c("Bonly")]),
      by = data[, tstratw_b, drop = FALSE],
      lenique, drop = dropmissing
    )
  )

  comp <- merge(
    stats::reshape(comp, timevar = "SEX", idvar = Cstrat, direction = "wide"),
    stats::aggregate(
      list("alltows" = data[, towid]),
      by = data[, towstrat, drop = FALSE],
      lenique, drop = dropmissing
    ),
    by = towstrat, all.x = TRUE
  )

  comp <- dplyr::left_join(comp, comp_2)

  comp <- comp[, colnames(comp) != "sexed"]
  colnames(comp)[colnames(comp) == "FREQ"] <- "FREQ.B"
  colnames(comp)[
    colnames(comp) == "Final_Sample_Size_L"
  ] <- "Final_Sample_Size_L.B"

  if (
    length(grep("ONLY_U_TOWS.F|ONLY_U_TOWS.M|ONLY_B_TOWS", colnames(comp))) >
      0) {
    comp <- comp[
      ,
      -grep("ONLY_U_TOWS.F|ONLY_U_TOWS.M|ONLY_B_TOWS", colnames(comp))
    ]
  }
  colnames(comp) <- gsub(
    "(.+)\\.([A-Z])",
    "\\L\\2\\1",
    colnames(comp),
    perl = TRUE
  )
  colnames(comp) <- gsub(
    "freq|freq.+",
    "samps",
    colnames(comp),
    ignore.case = TRUE
  )
  colnames(comp) <- gsub(
    "uonl.+",
    "ONLY_U_TOWS",
    colnames(comp),
    ignore.case = TRUE
  )
  colnames(comp) <- gsub(
    paste0("([a-z])", weightid),
    "\\1",
    colnames(comp),
    ignore.case = TRUE
  )
  colnames(comp) <- gsub("^f$", "female", colnames(comp))
  colnames(comp) <- gsub("^m$", "male", colnames(comp))
  colnames(comp) <- gsub("^u$", "unsexed", colnames(comp))
  colnames(comp) <- gsub("^b$", "both", colnames(comp))
  # todo: remove legacy code of needing fishyr
  comp[is.na(comp)] <- 0
  return(comp)
}

#' Return sample IDs that did not sex samples
#'
#' Identifiers that have female and male samples will be returned as NA and
#' only identifiers that had unsexed fish will be provided.
#' @param identifier Unique IDs for the samples.
#' @param sex A vector of the same length as `identifier` providing
#'   the sex of the sampled fish.
#' @param good A character value supplying the `sex` value you
#'   want to keep.
#'
#' @author Kelli F. Johnson
#' @return
#' A vector of identifiers that only had the sex given in the `good` argument.
#' The returned vector will be of the same length as the supplied vectors.
#' @noRd
#'
getunsexedsamps <- function(identifier, sex, good = "U") {
  if (inherits(sex, "factor")) sex <- as.character(sex)
  ff <- function(x) paste(unique(x), collapse = "")
  keep <- stats::ave(sex, identifier, FUN = ff)
  return(ifelse(keep == good, identifier, NA))
}

#' Number of unique entries
#'
#' A helper function that returns the number of unique entries in a vector.
#' Useful for apply functions.
#' @param x A vector of character or numeric values.
#' @author Kelli F. Johnson
#' @return
#' An integer value specifying the number of unique entries.
#' @noRd
lenique <- function(x) {
  return(length(unique(x)))
}
