#' Get the general area that the information was taken from
#'
#' Get the general area that the information (i.e., row of data) was taken from.
#' Thankfully, PacFIN contains records from all areas, but unfortunately, we do
#' not always want to use data from all of these areas in the stock assessment.
#' Thus, this function helps to identify which data should be included based on
#' standard filtering. We encourage users to continue to explore their data for
#' records that may be outside of their jurisdiction for further filtering.
#'
#' @details # Rationale for excluded areas
#'
#' ## Washington Department of Fish and Wildlife (WDFW)
#'
#' WDFW uses management regions, also known as Marine Fish-Shellfish Management
#' and Catch Reporting Areas, to define the area in which landings occurred and
#' sampled fish come from. These management regions are numeric and fall within
#' the range of `1:99`, though not all numeric values within this range are
#' actually used. WDFW uses their management regions to define the PSMFC area in
#' which landings occurred because PSMFC area is not a ``recorded'' value on
#' fish tickets. See Table 3 on page 15 of [Tsou et al.
#' (2015)](https://wdfw.wa.gov/sites/default/files/publications/01754/wdfw01754.pdf)
#' for a table that converts WDFW management region to PSMFC area.
#'
#' A partial map of the WDFW management regions for the Puget Sound areas is
#' provided in the [Puget Sound Groundfish Management
#' Plan](https://wdfw.wa.gov/sites/default/files/publications/00927/wdfw00927.pdf)
#' (Palsson et al., 1998; see Figure 3 on page 26), and an arcgis layer of the
#' management regions within the Strait of Juan de Fuca and the Puget Sound was
#' posted by
#' [SITC](https://www.arcgis.com/apps/webappviewer/index.html?id=13a09a6daae7408383fe3617ced75259).
#' Both of these resources were helpful in understanding how WDFW management
#' regions 20-29 correspond to PSMFC area 4A and should be attributed to the
#' Puget Sound. Where, 4A corresponds to everything east of Cape Flattery (i.e.,
#' the most easterly point of Washington state).
#'
#' ## Foreign
#'
#' Foreign samples, those coming from outside of the EEZ, could be in your data.
#' The PacFIN documentation for
#' [areas](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt)
#' identifies areas `ZY` or `ZZ` or `9[GMYZ]` as those outside of the EEZ.
#' Additionally, 3D and 3N make up VC, which is Canadian catches within the
#' Vancouver area.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#'
#' @export
#' @seealso
#' [cleanPacFIN()] uses `getArea()`.
#' @return
#' A vector of characters with the same length as `NROW(Pdata)`. Rows of data
#' that should potentially be removed from your data are given a grouping like
#' `"Puget Sound"` in an attempt to indicate where the record came from in a
#' more transparent way than cryptic codes like `4a`. `NA`s indicate records
#' that the package maintainers believe should be maintained in your data set.
#'
getArea <- function(Pdata,
                    verbose = TRUE) {
  out <- rep(NA, NROW(Pdata))

  # Find column names b/c they can be named different things depending on the
  # year the data were pulled and what kind of data are passed to Pdata
  column_numbers <- grep(
    pattern = "^PSMFC_|CATCH_AREA|CDFW_AREA_BLOCK",
    x = colnames(Pdata),
    ignore.case = TRUE
  )
  if (length(column_numbers) == 0) {
    stop("Your data does not have a column indicating the area.")
  }

  # Use unite() to paste (b/c it is faster than glue) all entries into a
  # single entry and return a vector
  strings <- tidyr::unite(
    Pdata,
    col = "new",
    tidyr::matches("^PSMFC_|CATCH_AREA|CDFW_AREA_BLOCK"),
    sep = " --- "
  ) %>%
    dplyr::pull(new)

  # Use PSMFC column to find Puget Sound area
  out[grepl("4A", strings, ignore.case = TRUE)] <- "Sound and Straits"

  # Find outside EEZ
  out[grepl("Z[yz]|9[gmyzu]", strings, ignore.case = TRUE)] <- "Outside EEZ"
  out[grepl("3[DN]", strings, ignore.case = TRUE)] <- "CAN (VNCVR)"
  out[grepl("5[a-z]", strings, ignore.case = TRUE)] <- "CAN"

  if (verbose && sum(!is.na(out)) != 0) {
    message(
      "\n The table below summarizes the number of records that are outside\n",
      " the area included in U.S. West Coast population assessments.\n",
      " Columns with information about area of landing were pasted together\n ",
      "and searched for specific strings indicative of an excluded region."
    )
    utils::capture.output(
      type = "message",
      dplyr::count(
        data.frame(region = out, strings),
        strings, region
      ) %>%
        dplyr::filter(!is.na(region))
    )
  }
  if (verbose && sum(!is.na(out)) == 0) {
    message(
      "\n No records were determined to be outside of the area included in\n",
      " U.S. West Coast population assessments. You should perform\n",
      " additional explorations to confirm this."
    )
  }
  return(out)
}
