#' Expand bds data up to the tow and catch level
#'
#' Calculate the first- and second-stage expansions. The first-stage expansion 
#' account for unsampled fish in the smallest measured unit. 
#' Where, in PacFIN data, the smallest measured unit typically a trip
#' because that is what is available to the port or dockside sampler. Whereas,
#' in survey data the smallest measured unit is typically a tow. Tow would be
#' the smallest if we had samples from onboard observers rather than from
#' dockside samplers. The second-stage expansion expands data up to the state 
#' or area catch level for that year and stratificiation. 
#' Find the catch for each year and grouping in `Catch` and divide by the
#' pounds of fish that were collected for sampling for that same year and
#' grouping. Sampled biomass is stored in `All_Trips_Sampled_Lbs`, which is
#' the sum of Trip_Sampled_Lbs across sample numbers.
#' Catches were already stratified (i.e., summed by group placed in a column
#' for a given year or row). Catches are converted to pounds prior to dividing.
#' Thus, per-stratum Expansion_Factor_2 is the catch / sampled catch.
#' The total expansion is the product of the first and second stage expansion.
#'
#' @export
#' @seealso
#' * [cleanPacFIN()] (upstream)
#' * [nwfscSurvey::estimate_weight_length()] (upstream())
#' * [getExpansion_1()] (contain within)
#' * [getExpansion_2()] (contain within)
#'
#' @details
#' The workflow is to run this function [cleanPacFIN(),
#' which assures that all of the necessary columns are available and that the
#' data are in the correct units. This function then calls two helper functions,
#' [EF1_Numerator()] and [EF1_Denominator()] to calculate the weight of sampled fish and the weight of all fish of the respective species in the tow, respectively. Finally, the ratio of the two values is returned.
#'
#' @section Expansion:
#' \itemize{
#' \item{Age data are expanded separately from lengths.}
#' \item{WA fish are generally only expanded using Expansion_Factor_2.}
#' \item{Expansions are the product of
#'   Expansion_Factor_1 * Expansion_Factor_2.
#' }
#' \item{For age-at-length comps, set Final_Expansion_Factor to 1 because
#'   each fish represents only itself.}
#' }
#'
#' @inheritParams cleanPacFIN
#' @param weight_length_estimates Dataframe of length-weight estimates with the 
#'   the following columns: sex, A, B. It is recommended to use to use 
#'   `nwfscSurvey::estimate_weight_length()` and to use survey data. 
#' @param maxExp The maximum expansion factor (either a number or a quantile)
#'   for building expansions. Typically, the default is 0.95. Set `maxExp =
#'   Inf` to see largest values.
#' @param Exp_WA A logical values specifying if the samples from Washington
#'   should be exanded. The default is `FALSE`.
#' @param Catch A data frame of catch data, in pounds or in metric tonnes.
#' @param Units The units of the \code{Catch} data frame, see
#'   \code{measurements::conv_unit_options[["mass"]]} for options. Typical units
#'   are metric tonnes (e.g., \code{"metric_ton"}) because that is the unit used
#'   in Stock Synthesis, but expansions are done in pounds because fish weights
#'   are in pounds. Thus, catches also need to be in pounds and will be
#'   converted as such.
#' @param stratification.cols A vector of column names in `Pdata` that you want
#'   to use as strata. These will match the way in which the catches are
#'   transformed from long to wide prior to inputting them into this function.
#'   If you leave this argument empty, then `Pdata` must already have a column
#'   named `stratification`. The function will look in the column names of the
#'   `Catch` data to determine the appropriate separator to use between columns
#'   when pasting the words together, which is done using [apply] and [paste].
#'   Historically, it was mandatory to make this column yourself, but in 2021,
#'   this input argument was added to reduce the number of extraneous calls that
#'   were needed between functions. You can use as many levels of stratification
#'   as you want except year because it is already included in the call to
#'   [stats::aggregate].
#' @template savedir
#'
#' @examples
#' \dontrun{
#' # Calculate the weight--length parameters for input to this function
#' bds_survey <- nwfscSurvey::pull_bio(
#'   common_name = "widow rockfish",
#'   survey = "NWFSC.Combo"
#'   )
#' pars <- nwfscSurvey::estimate_weight_length(
#'   data = bds_survey,
#'   col_length = "length_cm",
#'   col_weight = "weight_kg",
#'   verbose = FALSE
#'  )
#'  
#' expanded_comps <- get_pacfin_expansions(
#'   Pdata = bds_cleaned,
#'   Catch = catch_dataframe,
#'   weight_length_estimates = pars,
#'   Units = "MT",
#'   Comps = "LEN",
#'   maxExp = 0.95)
#' }
#' @return
#' A `data.frame` with expanded data up to the trip and total catch level.
#'
#'
get_pacfin_expansions <- function(
    Pdata,
    Catch,
    weight_length_estimates,
    stratification.cols,
    Units = "MT",
    maxExp = 0.95,
    Exp_WA = TRUE,
    verbose = TRUE,
    savedir = NULL) {
  nwfscSurvey::check_dir(dir = savedir, verbose = verbose)
  
  # Check weight-length data frame
  required_cols <- c("sex", "A", "B")
  if (any(!required_cols %in% colnames(weight_length_estimates))){
    cli::cli_abort(
      "The weight_length_estimates dataframe is required to have three columns 
    named {required_cols}")
  }
  required_sexes <- c("female", "male", "all") 
  if (any(!required_sexes %in% weight_length_estimates[,"sex"])){
    cli::cli_abort(
      "The weight_length_estimates dataframe is required to have three rows in  
    the sex column for {required_sexes}")
  }
  if (verbose) {
    cli::cli_inform(
      "The units for the Catch dataframe is specified to be in {Units}"
    )
    if (Units == "MT") {
      cli::cli_inform(
        "Catches will be converted to pounds for the second-stage expansion."
      )
    }
  }
  fa = weight_length_estimates |>
    dplyr::filter(sex == "female") |>
    dplyr::pull("A")
  fb = weight_length_estimates |>
    dplyr::filter(sex == "female") |>
    dplyr::pull("B")
  ma = weight_length_estimates |>
    dplyr::filter(sex == "male") |>
    dplyr::pull("A")
  mb = weight_length_estimates |>
    dplyr::filter(sex == "female") |>
    dplyr::pull("B")
  ua = weight_length_estimates |>
    dplyr::filter(sex == "all") |>
    dplyr::pull("A")
  ub = weight_length_estimates |>
    dplyr::filter(sex == "all") |>
    dplyr::pull("B")
    
  data_exp1 <- getExpansion_1(
    Pdata = Pdata,
    fa = fa,
    fb = fb,
    ma = ma,
    mb = mb,
    ua = ua,
    ub = ub,
    maxExp = maxExp,
    Exp_WA = Exp_WA,
    verbose = verbose,
    savedir = savedir)
    
  # Check for the "stratification" column
  if (length(data_exp1$stratification) == 0) {
    if (!missing(stratification.cols)) {
      if (all(stratification.cols %in% colnames(data_exp1))) {
        if (length(stratification.cols) == 1) {
          data_exp1[, "stratification"] <- data_exp1[, stratification.cols]
        } else {
          separate <- unique(gsub(
            "^[a-zA-Z]+(\\s*[[:punct:]]\\s*)[a-zA-Z]+$",
            "\\1", colnames(Catch)[-1]
          ))
          data_exp1[, "stratification"] <- apply(data_exp1[, stratification.cols],
                                                 1, paste,
                                                 collapse = separate
          )
        }
      } else {
        cli::cli_abort(
          "Pdata must have stratification column or provide
        {.var stratification.cols}."
        )
      }
    }
  } # End if
    
  data_exp2 <- getExpansion_2(
    Pdata = data_exp1,
    Catch = Catch,
    Units = Units,
    maxExp = maxExp,
    verbose = verbose,
    savedir = savedir
  )
  
  data_exp2[["Final_Sample_Size_L"]] <- capValues(
    data_exp2[["Expansion_Factor_1_L"]] * data_exp2[["Expansion_Factor_2"]],
    maxVal = expansion
  )
  data_exp2[["Final_Sample_Size_A"]] <- capValues(
   data_exp2[["Expansion_Factor_1_A"]] * data_exp2[["Expansion_Factor_2"]],
   maxVal = expansion
  )
  data <- data_exp2 
  return(data)
}
