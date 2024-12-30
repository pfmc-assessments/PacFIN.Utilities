#' Write out composition data formatted for Stock Synthesis
#'
#' Write out composition data to a file, binning the data as specified to work
#' in a Stock Synthesis model. Note, that this function does not work for
#' conditional age-at-length data because it is assumed that one would not want
#' to estimate growth in that way from fishery-dependent data.
#'
#' @export
#'
#' @param inComps A dataframe returned from [getComps()].
#' @param fname A filename with the appropriate extension, used to save the
#'   function output to the disk. For example, `LenComps.csv` or
#'   `file.path(getwd(), "SampleSize.csv")`. Full, relative, or simple paths are
#'   allowed because the argument is used as is, i.e., not redirected to a
#'   directory different than [getwd()]. Note that various functions within
#'   PacFIN.Utilities have different default values for this input argument. If
#'   `NULL` in `writeComps`, then the resulting file name will be based on what
#'   type of composition data is being generated, i.e., `PacFIN_lengths.out`,
#'   or `PacFIN_ages.out` for length or age data, respectively.
#' @param abins,lbins Deprecated as of 0.2.10 to reduce complication in the code and
#'   make it more intuitive for the user when running this function.
#'   The binning structure to use for ages and lengths. For
#'   both arguments, the default is `NULL` which leads to the natural bins of
#'   the data being used, i.e., no additional binning is performed.
#' @param comp_bins The binning structure to use for ages and lengths. The default 
#'   is `NULL` which leads to the natural bins of the data being used, i.e.,
#'    no additional binning is performed.
#' @param column_with_input_n A string providing the column name with the
#'   appropriate value for the input sample size that will be given to Stock
#'   Synthesis as input_n (what we and {nwfscSurvey} provide as a column name)
#'   or NSamp (what Stock Synthesis wants as a name). The default is `"n_tows"`,
#'   which is the number of tows that were performed for the given
#'   stratifications passed to [getComps()] that created `inComps`. One might
#'   want to use `"n_fish"` or some column that you created yourself.
#' @param month Month for all observations. Defaults to 7. If input has multiple
#'   seasons, this must be a vector of equal length to the maximum seasons where
#'   the order of months in the vector will be assigned to season in ascending
#'   order. For example, if there  are two seasons and the month = c(1, 7)
#'   season 1 will be assigned to month 1 and season 2 to month 7.
#' @param partition  Used by Stock Synthesis for length- or age-composition data
#'   where 0 = retained + discarded, 1 = discarded, and 2 = retained fish. The
#'   default is to assume that these fish are retained only. The default was
#'   changed in 2020 from a value of 0, and code should be updated accordingly
#'   if you really want 0.
#' @param ageErr Defaults to 1 to use the first ageing error matrix specified in
#'   your Stock Synthesis model.
#' @param digits Integer indicating the number of decimal places to round value
#'   to (value is passed to [round()]). The default is 4, which will lead to
#'   rounding to four digits. If you do not want to round the results, then use
#'   `Inf`, which will lead to the use of R's default precision.
#' @param maxAge Deprecated as of 0.2.10 to reduce complication in the code and
#'   make it more intuitive for the user when running this function. All fish
#'   present in `inComps` will be binned according to the bins provided or the
#'   bins present.
#' @param dummybins Deprecated as of version 0.2.10 because you can just run the
#'   function with different `abins` or `lbins` rather than this function
#'   changing the first and end bin for you. The original default, years ago was
#'   `TRUE`, then it was changed to `FALSE`. Now, without this argument, it is
#'   essentially `FALSE`.
#' @param sum1 Deprecated as of version 0.2.10 because SS3 will normalize the
#'   composition data for you and thus it does not need to be done here. The
#'   previous default was `FALSE` so the removal of this argument does not
#'   change the default behavior of this function.
#' @param overwrite Deprecated as of version 0.2.10. The file will be
#'   automatically overwritten if present.
#' @inheritParams cleanPacFIN
#'
#' @details
#'
#'   The structure of the input dataframe determines whether the results
#'   produce age- or length-composition data. As of version 0.2.10, conditional
#'   age-at-length compositions are no longer available within this function.
#'
#'   Composition data are raw weights rather than proportions. Stock Synthesis
#'   internally converts these to proportions.  The raw weights should be
#'   examined for anomalies. To create proportions use [prop.table()] on the
#'   columns containing composition data.
#'
#'   The columns in the output preceding 'lengthcm' or 'Age' are those that
#'   were used in stratifying the data.
#' @return
#' A data frame is returned and a `.csv` is created and written to the disk with
#' the same data frame.
#'
#' @author Andi Stephens, Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
#' @seealso
#' * `getComps()`
#'
writeComps <- function(inComps,
                       fname = NULL,
                       abins = lifecycle::deprecated(),
                       lbins = lifecycle::deprecated(),
                       comp_bins = NULL,
                       column_with_input_n = "n_tows",
                       maxAge = lifecycle::deprecated(),
                       month = 7,
                       partition = 2,
                       ageErr = 0,
                       dummybins = lifecycle::deprecated(),
                       sum1 = lifecycle::deprecated(),
                       digits = 4,
                       overwrite = lifecycle::deprecated(),
                       verbose = FALSE) {
  # lifecycle checks
  if (lifecycle::is_present(overwrite)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(overwrite)",
      details = "File is automatically overwritten."
    )
  }
  if (lifecycle::is_present(sum1)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(sum1)",
      details = "Composition data are not normalized because SS3 will do this."
    )
  }
  if (lifecycle::is_present(dummybins)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(dummybins)",
      details = "Just run the function with different abins or lbins."
    )
  }
  if (lifecycle::is_present(maxAge)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(maxAge)",
      details = "The bins are not truncated."
    )
  }
  if (lifecycle::is_present(abins)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(abins)",
      details = "Please use comp_bins."
    )
  }
  if (lifecycle::is_present(lbins)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "writeComps(lbins)",
      details = "Please use comp_bins."
    )
  }
  # Check inputs
  if ("season" %in% names(inComps) &&
      max(inComps[["season"]]) != length(month)) {
    cli::cli_abort(c(
      "i" = "Input 'month' should have length equal to the maximum season",
      "x" = "month = {.var {month}}",
      "x" = "seasons = {sort(unique(inComps[['season']]))}"
    ))
  }
  if (!"season" %in% names(inComps) &&
      length(month) != 1) {
    cli::cli_abort(c(
      "x" = "month should have length 1 instead of length {length(month)} 
        because {.var season} does not exist as a column in {.var inComps}"
    ))
  }
  if (!"fishyr" %in% colnames(inComps)) {
    cli::cli_abort("fishyr should be a column")
  }
  if (!"fleet"  %in% colnames(inComps)) {
    cli::cli_abort("fleet should be a column")
  }
  if (!column_with_input_n  %in% colnames(inComps)) {
    cli::cli_abort("{.var {column_with_input_n}} should be a column")
  }
  
  Names <- names(inComps)
  AGE <- which(Names == "Age")
  LEN <- which(Names == "lengthcm")
  # Which comps are we doing, where either Age or lengthcm must be present in
  # inComps and both will be present for conditional data?
  if (length(AGE) + length(LEN) == 0) {
    cli::cli_abort("lengthcm or Age are not columns in {.val inComps}")
  }
  
  # Create fname if it is not give based on what types of comps we are doing
  if (is.null(fname)) {
    fname <- dplyr::case_when(
      length(LEN) > 0 ~ glue::glue("PacFIN_length_comps_{min(comp_bins)}-{max(comp_bins)}.csv"),
      length(AGE) > 0 ~ glue::glue("PacFIN_age_comps_{min(comp_bins)}-{max(comp_bins)}.csv")
    )
  }
  if (verbose) {
    cli::cli_bullets(c(
      "*" = "Writing composition data to {fname}."
    ))
  }
  fs::dir_create(
    path = dirname(normalizePath(fname, mustWork = FALSE)),
    recurse = TRUE
  )

  type_loc <- ifelse(
    length(AGE) > 0,
    yes = AGE,
    no = LEN
  )
  colnames(inComps)[type_loc] <- "comp_type"
  
  # Modify inComps to include all bins in comp_bins
  check_bin_width <- diff(comp_bins)
  if (any(check_bin_width != check_bin_width[1])) {
    cli::cli_inform(
      "The output should be careful checked to ensure correctness when unequal
      bin intervals are used."
    )
  }
  bin_width <- check_bin_width[1]
  grid <- inComps |>
    tibble::tibble() |>
    tidyr::expand(fishyr, fleet, season, SEX, tidyr::full_seq(comp_bins, bin_width))
  colnames(grid)[ncol(grid)] <- "comp_type"
  expanded_comps <- inComps |>
    dplyr::right_join(grid) |>
    tibble::tibble() |>
    tidyr::complete(fishyr, fleet, season, comp_type, 
                    fill = list(
                      n_tows = 0, 
                      n_fish = 0,
                      comp = 0
                    )
    )

  if (is.null(comp_bins)) {
    if (verbose) {
      cli::cli_alert_info("No composition bins provided, using data as-is.")
    }
    comp_bins <- sort(unique(inComps[["comp_type"]]))
  } 
  
  bins <- c(comp_bins, Inf)
  # add extra, dummy bin because all.inside = TRUE
  expanded_comps$bin <- findInterval(expanded_comps[["comp_type"]], bins, all.inside = TRUE)
  target <- "bin"
  key_names <- c(Names[1:(type_loc - 1)])

  # letter to paste with the bin to make f1 f2 f3 m1 m2 m3 for
  # a two sex model or u1 u2 u3 if just unsexed fish
  sex_label_left_side <- dplyr::case_when(
    all(c("M", "F", "U") %in% inComps[["SEX"]]) ~ "f",
    "F" %in% inComps[["SEX"]] ~ "f",
    "U" %in% inComps[["SEX"]] ~ "u"
  )
  
  wide_composition_data <- expanded_comps |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(
        c(key_names, column_with_input_n, "SEX", target)
      ))
    ) |>
    dplyr::summarize(comp = round(sum(comp), digits = digits)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Create the f1 f2 ... m1 m2 ... or u1 u2 ... labels to move to wide
      # columns later
      sex_length = sprintf(
        fmt = "%s%05d",
        ifelse(SEX == "U", sex_label_left_side, tolower(SEX)),
        get(paste0(target, "s"))[!!dplyr::sym(target)]
      ),
      # Relabel males as females in sex so they get cast to the right when
      # making a wide data frame
      SEX = ifelse(SEX == "M", "F", SEX)
    ) |>
    dplyr::arrange(fleet, sex_length) |>
    tidyr::pivot_wider(
      id_cols = c(key_names, column_with_input_n, "SEX"),
      names_from = "sex_length",
      values_from = "comp",
      names_sort = TRUE,
      values_fill = 0
    ) |>
    dplyr::arrange(SEX) |>
    dplyr::mutate(
      season = factor(season, labels = month),
      # Males and females with sex-ratio preserved are 3 and unsexed
      # fish with males and females combined are 0 in a two-sex model
      SEX = ifelse(SEX == "F", 3, 0),
      partition = partition
    ) |>
    dplyr::rename(
      "sex" = "SEX",
      "month" = season,
      year = fishyr,
      input_n = column_with_input_n
    ) |>
    dplyr::filter(input_n > 0) |>
    dplyr::relocate(fleet, sex, partition, .after = month) |>
    dplyr::arrange(fleet, sex, year) |>
    dplyr::rename_with(.fn = \(x) gsub("([a-z])0+([1-9])", "\\1\\2", x))
  
  if (length(AGE) > 0) {
    returned_composition_data <- wide_composition_data |>
      dplyr::mutate(
        ageerr = ageErr,
        Lbin_lo = -1,
        Lbin_hi =  -1,
        .after = partition
      )
  } else {
    returned_composition_data <- wide_composition_data
  }

  utils::write.table(
    file = fname,
    returned_composition_data,
    sep = ",",
    col.names = TRUE,
    row.names = FALSE,
    append = FALSE
  )

  invisible(returned_composition_data)
} # End function writeComps
