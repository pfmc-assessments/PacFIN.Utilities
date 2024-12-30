#' Calculate weight--length relationship parameters
#'
#' Estimate parameters of the weight--length relationship for each
#' sex and all sexes combined, where the latter includes unsexed fish.
#'
#' @param data A data frame containing empirical weights and lengths
#' from sampled fish.
#' Sexes should be available in the column `sex` or `SEX`.
#' @param col.length A numeric or character value specifying the column
#' to use in `data` for length information. These lengths are assumed to
#' be in centimeters. The default value is `lengthcm`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' @param col.weight A numeric or character value specifying the column
#' to use in `data` for weight information. These weights are assumed to
#' be in kilograms The default value is `weightkg`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' Using kilograms is the default because Stock Synthesis assumes the
#' weight--length parameters are calculated using centimeters and kilograms.
#' The reported values are easily scaled to give you results in grams if
#' you wish to have more standard parameter estimates.
#' @inheritParams cleanPacFIN
#'
#' @author Kelli F. Johnson
#' @export
#' @return A data frame of weight--length parameters by sex.
#' Parameters A and B are in the appropriate units to input
#' into Stock Synthesis Wtlen_1_Fem and Wtlen_2_Fem, or
#' Wtlen_1_Mal and Wtlen_1_Mal, parameters in the
#' control file. Values of `NA` are returned for models that
#' did not have enough data to properly estimate the parameters.
#' This will happen when there are no females in your data set,
#' for example.
#'
getWLpars <- function(data,
                      col.length = "lengthcm",
                      col.weight = "weightkg",
                      verbose = TRUE) {
  col.length <- tolower(col.length)
  col.weight <- tolower(col.weight)
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub(col.weight, "weight", colnames(data))
  colnames(data) <- gsub(col.length, "length_cm", colnames(data))
  col.length <- "length_cm"
  col.weight <- "weight"
  stopifnotcolumn(data = data, string = col.length)
  stopifnotcolumn(data = data, string = col.weight)
  stopifnotcolumn(data = data, string = "sex")

  dims <- dim(data)
  data <- data[
    !is.na(data[[col.weight]]) &
      !is.na(data[[col.length]]),
  ]
  if (verbose) {
    message(
      "Calculating the weight--length relationship from ",
      nrow(data), "\nfish because ", dims[1] - nrow(data),
      " fish did not have empirical weights and lengths."
    )
  }

  mresults <- tibble::lst(
    female = . %>% dplyr::filter(sex == "F"),
    male = . %>% dplyr::filter(sex == "M"),
    all = . %>% dplyr::filter(sex %in% c(NA, "F", "M", "U", "H"))
  ) %>%
    purrr::map_dfr(~ tidyr::nest(.x(data), data = everything()), .id = "group") %>%
    dplyr::mutate(
      fits = purrr::map(data, ~ stats::lm(log(weight) ~ log(length_cm), data = .x))
    )
  WLresults <- mresults %>%
    dplyr::summarize(
      group = group,
      median_intercept = purrr::map_dbl(fits, ~ exp(.x$coefficients[1])),
      SD = purrr::map_dbl(fits, ~ sd(.x$residuals)),
      A = purrr::map_dbl(fits, ~ exp(.x$coefficients[1]) * exp(0.5 * sd(.x$residuals)^2)),
      B = purrr::map_dbl(fits, ~ .x$coefficients[2])
    ) %>%
    data.frame()

  if (verbose) {
    message("weight--length model results by SEX:")
    utils::capture.output(lapply(mresults[["fits"]], summary), type = "message")
  }

  return(WLresults)
}
