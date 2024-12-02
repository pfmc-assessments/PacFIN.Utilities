#' Aggregate length, age, or age-at-length composition data by strata
#'
#' `getComps()` first sets up the local environment and returns summaries by
#' groupings.
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
#' @param towid A vector of character values providing the column names that
#'   generate a unique id for each sample. The default is `"SAMPLE_NO"` but you
#'   can pass a vector where all of the columns identified will be united to
#'   create a unique identifier for the tow/sample.
#' @param weightid A character value giving the column name that holds the value
#'   to be summed for each type and strata, such as `weightid =
#'   "Final_Sample_Size_L"`, if you are interested in length data. Instead of
#'   the default, you might want to weight each stratification by the final
#'   sample sizes found for age data, i.e., `weightid = "Final_Sample_Size_A"`.
#'   You can pass `weightid` any column that exists in `Pdata`. Legacy code did
#'   not allow you to name the column and `Final_Sample_Size` was hard coded in
#'   the function.
#' @return
#' A long data frame of weights for each grouping structure. Columns identifying
#' the grouping structures will come first, followed by columns with sample
#' sizes per group by sex category. The documentation for these sample size
#' columns is sparse because this function is set to be deprecated next cycle
#' and replaced with a simplified path to writing composition data. But,
#' information is present for males (abbreviated with an M), females
#' (abbreviated with an F), unsexed (abbreviated with a U). If no sex is
#' provided then it is assumed all are unsexed and will be returned as such.
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
                     towid = c("SAMPLE_NO"),
                     weightid = "Final_Sample_Size_L") {
  # Set up stratification
  usualSuspects <- defaults

  # Avoid duplication
  strat <- strat[!strat %in% usualSuspects]
  Comps <- match.arg(Comps)
  towstrat <- c(
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
  type <- utils::tail(usualSuspects, 1)

  if (!all(towstrat %in% colnames(Pdata))) {
    cli::cli_abort("Not all towstrat are available.")
  }
  if (!type %in% colnames(Pdata)) {
    cli::cli_abort("{.var {type}} isn't a column in Pdata.")
  }
  # Only a column named "SEX" works in all caps
  good_column_for_sex <- grep("SEX", colnames(Pdata))
  if (length(good_column_for_sex) == 0) {
    Pdata[, "SEX"] <- "U"
    cli::cli_bullets(c(
      "x" = "SEX (case-specific) was missing from the column names of Pdata.",
      "i" = "A SEX column was added with all rows set to 'U' for unsexed fish."
    ))
  }
  # FREQ... stores the number of fish that sum to the weightid
  freqn <- grep("FREQ", colnames(Pdata), value = TRUE)
  if (length(freqn) == 0) {
    cli::cli_abort("FREQ is missing from the Pdata.")
  }

  # Create a unique id for each sample where this most often is just the
  # SAMPLE_NO
  # apply is faster than dplyr::*
  Pdata[, "uniqueid"] <- apply(
    Pdata[, towid, drop = FALSE],
    1,
    paste,
    collapse = "_"
  )

  # Determine the number of tows for each combination of sex available
  Pdata |>
    # By stratification variable count # of tows
    dplyr::group_by(dplyr::across(dplyr::all_of(towstrat))) |>
    dplyr::mutate(n_tows = dplyr::n_distinct(uniqueid)) |>
    # By stratification and SEX count number of sampled fish
    dplyr::group_by(SEX, .add = TRUE) |>
    dplyr::mutate(n_fish = sum(FREQ)) |>
    # By stratification, sex, and bin value count the weight
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(towstrat, type, "SEX"))
    )) |>
    dplyr::mutate(dplyr::across(c(weightid), .fns = sum)) |>
    # Get rid of extraneous columns
    dplyr::select(
      dplyr::all_of(c(towstrat, type)),
      n_tows,
      SEX,
      n_fish,
      weightid
    ) |>
    # Remove duplicated rows
    dplyr::distinct() |>
    # Give n_fish by sex and weight by sex in 6 separate columns
    dplyr::rename(comp = weightid)
    #  |>
    # tidyr::pivot_wider(
    #   names_from = SEX,
    #   values_from = c("n_fish", "comp"),
    #   values_fill = 0
    # )
}
