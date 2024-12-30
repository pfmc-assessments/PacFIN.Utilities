#' Get age information for each fish
#'
#' Get age information for each fish, or row of the data. This functionality was
#' originally in `cleanAges()`, moved to [cleanPacFIN()], and is now
#' compartmentalized here, but called in [cleanPacFIN()] and stored in the `Age`
#' column.
#'
#' @details
#' ## `FISH_AGE_YEARS_FINAL`
#' `FISH_AGE_YEARS_FINAL` is defined in PacFIN as the ``age of specimen (best
#' age or final age)''. If `FISH_AGE_YEARS_FINAL` does not have an entry, where
#' this should be the best age available, then the user is warned that they
#' should potentially look at the age reads from an individual reader, e.g.,
#' `age1` or `age2`, until the state works out what age read is best and
#' provides this in `FISH_AGE_YEARS_FINAL`.
#'
#' How `FISH_AGE_YEARS_FINAL` is determined can be unclear because it does not
#' always equal the value from one of the age readers. How the value is
#' determined is specific to the lab that conducted the ageing. Sometimes,
#' reconciling multiple age reads to determine the best age will lead to the
#' final age not equaling any of the individual age reads. For example, readers
#' can get together to determine the best read. We assume that those who
#' populated the column know what they are doing; but, please feel free to
#' report errors in PacFIN to GitHub issues or the package maintainer after
#' contacting the state representative.
#'
#' Ageing methods listed in keep (or derivatives of those) will be used to
#' filter for ages that are of desired method. Any age that was not estimated
#' using one of the desired methods will be replaced with an `NA`.
#'
#' ### Deprecated methods
#' Previously, `getAge()` averaged over multiple reads if there was no entry in
#' `FINAL_FISH_AGE_IN_YEARS`. This functionality has since been removed from the
#' package starting with version 0.2.6.
#'
#' ### Washington
#' Washington uses the following order of operations to assign the best age
#' given there are multiple reads. See [issue
#' #49](http://www.github.com/pfmc-assessments/PacFIN.Utilities/issues/49) for a
#' semi-detailed discussion. Note, that they will never report more than three
#' reads because that is the limitation of their database.
#' * age1 if only one read;
#' * randomly chosen age between age-read 1 and age-read 2 if the difference
#'   between the two reads is less than or equal to three;
#' * if the difference between two reads is more than three, then the best age
#'   is manually keyed in from that designated by a human;
#' * if three reads are equal, then the value is automatically assigned; and
#' * if there are three reads and they are not equal then an age reader must
#'   choose the final value.
#'
#' ### Oregon
#' Oregon employs a single age reader, or at least has since 2014. Twenty
#' percent of first reads are re-read by the same reader but without knowing any
#' information about the results of the first read.
#' * age1 if only one read;
#' * age1 if age1 and age2 agree;
#' * age3 if age1 and age2 do not agree, where age3 is not a blind read and the
#'   reader knows the values reported in age1 and age2 when reading the otolith
#'   for a third time.
#'
#' ### California
#' Ageing of otoliths collected in California is performed by the Cooperative
#' Ageing Project, funded through a grant with the Pacific States Marine
#' Fisheries Commission. This same lab also ages the samples collected on the
#' coast-wide surveys as well as some of the samples collected by Oregon when
#' there are too many ages to read for their single age reader.
#' * age1 if only one read;
#' * age1 if age1 and age2 agree;
#' * for younger fish, i.e., less than or equal to age 10, discrepancies can
#'   normally be resolved by looking at the otolith and having knowledge of both
#'   previous reads;
#' * for older fish, i.e., older than age 10, discrepancies are resolved and the
#'   resolved age is used;
#' * for older fish where the discrepancy cannot be resolved, age1 is used
#'   because it had the freshest otolith and potentially used the better half.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#' @param keep A vector of strings, integers, `NA`, and empty strings (e.g.,
#'   `""`) representing the `AGE_METHOD`s you want to keep. All entries will be
#'   converted to character values using [codify_age_method()]. To keep the best
#'   age for a given row, at least one of the ageing methods needs to have been
#'   performed using one of the methods present in this argument. Often, it is
#'   helpful to run [getAgeMethod()] on the relevant data prior to running this
#'   function to see what age methods are currently present.
#' @param col.bestage Deprecated in version 0.2.5.
#'
#' @export
#' @author Kelli F. Johnson
#'
#' @return
#' A vector of integer ages in years the same length as the number of rows
#' present in `Pdata`.
#'
getAge <- function(Pdata,
                   verbose = TRUE,
                   keep,
                   col.bestage = lifecycle::deprecated()) {
  if (lifecycle::is_present(col.bestage)) {
    lifecycle::deprecate_soft(
      when = "0.2.5",
      what = "getAge(col.bestage)"
    )
  }
  if ("AGE_IN_YEARS" %in% colnames(Pdata)) {
    stop(
      "`getAge()` only works with data pulled by `PullBDS.PacFIN()`, ",
      "which moves double reads to columns ",
      "rather than leaving them in rows within AGE_IN_YEARS.",
      "Please change `Pdata` to recent output from `PullBDS.PacFIN()."
    )
  }
  keep <- unique(codify_age_method(keep))
  column_with_age <- grep_final_age(colnames(Pdata))
  # Select relevant columns because this function only returns a vector not a
  # data frame so we don't need to keep all the columns.
  Pdata <- dplyr::select(
    .data = Pdata,
    dplyr::matches(
      paste("AGE", "FISH_ID", column_with_age, sep = "|"),
      ignore.case = TRUE
    )
  )

  # Return early if all ages are NA
  data_ages <- dplyr::select(
    .data = Pdata,
    dplyr::starts_with("age", ignore.case = FALSE),
    dplyr::contains(column_with_age)
  )
  if (all(is.na(data_ages))) {
    if (verbose) {
      cli::cli_warn("No ages were found in Pdata.")
    }
    return(rep(NA, length = NROW(Pdata)))
  }

  # Standardize all columns that have AGE_METHOD, AGE_METHOD2, ...
  Pdata <- Pdata |>
    dplyr::mutate(dplyr::across(
      dplyr::matches("AGE_M"),
      .fns = codify_age_method
    ))

  for (iiname in grep("AGE_METHOD[0-9]*$", colnames(Pdata), value = TRUE)) {
    iinum <- gsub("[a-zA-Z_]", "", iiname)
    Pdata[, paste0("age", iinum)] <- ifelse(
      test = Pdata[, iiname] %in% keep,
      yes = Pdata[, paste0("age", iinum)],
      no = NA
    )
  }

  # Not sure why entries would have an age but not have final age
  # 1 California sablefish emailed Brenda on 2023-04-07
  # 8300 WA records of various spp emailed Theresa on 2021-03-06 and emailed
  # Kristen H. again on 2023-04-07 to remind her of the discussion where
  # she had responded and said she was checking with IT
  all_methods <- Pdata |>
    tidyr::unite(
      all_methods,
      dplyr::matches("^AGE_METHOD"),
      sep = "--"
    ) |>
    dplyr::pull(all_methods)
  # Remove Age if none of the available age methods are in keep
  out <- dplyr::case_when(
    grepl(paste0(keep, collapse = "|"), all_methods) ~ Pdata[[column_with_age]],
    TRUE ~ NA
  )

  if (verbose) {
    # Make summary objects
    text_age_methods <- Pdata |>
      dplyr::select(dplyr::starts_with("AGE_M")) |>
      unlist() |>
      unique() |>
      sort(na.last = TRUE) |>
      sQuote() |>
      glue::glue_collapse(sep = ", ", last = " and ")
    table_summary <- dplyr::mutate(
      .data = Pdata,
      final_age = out,
      age = rlang::eval_tidy(dplyr::sym(column_with_age))
    ) |>
      dplyr::count(age, final_age) |>
      dplyr::arrange(final_age)
    table_summary_na <- dplyr::filter(
      .data = table_summary,
      is.na(final_age) &
        !is.na(age)
    ) |>
      dplyr::select(-final_age)
    table_summary_n_age <- dplyr::filter(
      .data = table_summary,
      !is.na(final_age)
    ) |>
      dplyr::select(, -age) |>
      tidyr::complete(
        final_age = 0:max(final_age, na.rm = TRUE),
        fill = list(n = 0)
      ) |>
      dplyr::pull(n) |>
      paste(collapse = ", ")
    text_n_na <- if (NROW(table_summary_na) > 0) {
      apply(
        table_summary_na,
        MARGIN = 1,
        FUN = function(x) sprintf("age-%d (n = %d)", x[1], x[2])
      )
    } else {
      "0"
    }
    text_n_missing_final <- tidyr::unite(
      data = Pdata,
      col = "all_ages",
      dplyr::matches("^age[0-9]"),
      na.rm = TRUE,
      sep = ""
    ) |>
      dplyr::filter(
        is.na(rlang::eval_tidy(dplyr::sym(column_with_age))),
        all_ages != ""
      ) |>
      NROW() |>
      {
        glue::glue("{.} rows were missing a final age")
      }
    names(text_n_missing_final) <- ifelse(
      substr(text_n_missing_final, 1, 1) == "0",
      "v",
      "x"
    )

    # Print messages to users
    cli::cli_bullets(c(
      " " = "{.fn getAge} summary information -",
      text_n_missing_final,
      "i" = glue::glue("
        The distribution (in numbers) for fish aged
        0--{max(out, na.rm = TRUE)} years is {table_summary_n_age}
      "),
      "i" = "Age methods {text_age_methods} were present",
      "i" = glue::glue("
        Age methods
        {glue::glue_collapse(sQuote(keep), sep = ', ', last = ' and ')}
        were desired
      "),
      "i" = "{sum(table_summary_na[['n']])} ages used undesired age methods",
      "v" = glue::glue("
        Number of ages by age (years) changed to `NA` is
        {paste(text_n_na, collapse = ', ')}
      ")
    ))
  }

  return(out)
}

#' Find the column with the best age information
#'
#' Search a vector of strings for `"FINAL_FISH_AGE_IN_YEARS"` or
#' `"FISH_AGE_YEARS_FINAL"`.
#'
#' @param x A character vector.
#' @author Kelli F. Johnson
#' @return
#' A single character object.
grep_final_age <- function(x) {
  all <- grep(
    pattern = "AGE.*FINAL|FINAL.*AGE",
    x = x,
    value = TRUE
  )
  out <- all[!grepl(pattern = "CODE", x = all)]
  if (length(out) == 0) {
    stop("A match to AGE.*FINAL|FINAL.*AGE was not found.")
  }
  if (length(out) > 1) {
    stop(
      "Please remove all but one of the following columns and rerun the\n",
      " function:\n",
      paste(out, collapse = "\n")
    )
  }
  return(out)
}
