#' Get the general area that the information was taken from
#'
#' Get the general area that the information (i.e., row of data) was taken from.
#' Thankfully, PacFIN contains records from all areas, but unfortunately,
#' we do not always want to use data from all of these areas in the stock assessment.
#' Thus, this function helps to identify which data should be included based on
#' standard filtering. More in-depth filtering will be case specific and should
#' be performed by the user.
#'
#' @details
#' Washington Department of Fish and Wildlife (WDFW) uses management regions, also
#' known as Marine Fish-Shellfish Management and Catch Reporting Areas, to define
#' the area in which landings occurred and sampled fish come from.
#' These management regions are numeric and fall within the range of `1:99`.
#' Not all numeric values within this range are actually used, but they are helpful
#' in that WDFW uses their management regions to define the PSMFC area in which
#' landings occurred because PSMFC area is not a "recorded" value on fish tickets.
#' See Table 3 on page 15 of
#' [Tsou et al., 2015](https://wdfw.wa.gov/sites/default/files/publications/01754/wdfw01754.pdf)
#' for a table that converts WDFW management area to PSMFC area.
#' I had a difficult time finding a map of the 99 management areas, though a partial
#' map of the Puget Sound areas is provided in Figure 3 on page 26 of the
#' Puget Sound Groundfish Management Plan by
#' [Palsson et al., 1998](https://wdfw.wa.gov/sites/default/files/publications/00927/wdfw00927.pdf)
#' and an arcgis layer of the areas within the Strait of Juan de Fuca and the Puget Sound
#' was posted by [SITC](https://www.arcgis.com/apps/webappviewer/index.html?id=13a09a6daae7408383fe3617ced75259).
#' Both of these resources were helpful in understanding how WDFW management areas 20-29
#' correspond to PSMFC area 4a and should be attributed to the Puget Sound.
#' Where, 4a corresponds to everything east of Cape Flattery
#' (i.e., the most easterly point of Washington state).
#'
#' Foreign samples, those coming from outside of the EEZ, could be in your data.
#' The PacFIN documentation for
#' [areas](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt)
#' identifies areas ZY or ZZ or 9[GMYZ] as those outside of the EEZ.
#' Additionally, 3D and 3N make up VC, which is Canadian catches within the Vancouver area.
#' @template Pdata
#' @template verbose
#'
#' @export
#' @seealso [cleanPacFIN] uses this function to get the general area and alert users
#' which rows should be excluded from their data.
#' @return A vector of character values, where NA entries are fine with respect to
#' what Kelli F. Johnson determined as being 'fine' and character entries attempt to
#' specify the poor area from which the record originated from. For example,
#' "Puget Sound" clearly indicates that the record was from the Puget Sound, an area
#' that most stocks do not include in their spatial footprint.
#' This vector will be of the same length as the number of rows of the input data frame.
#'
getArea <- function(Pdata, verbose = FALSE) {
  out <- rep(NA, NROW(Pdata))

  # Find column names b/c they can be named different things
  PSMFCcol <- grep("^PSMFC_", colnames(Pdata))
  if (length(PSMFCcol) > 1) {
    if (verbose) {
      warning("More than one PSMFC_ column was found, e.g.,\n",
        knitr::combine_words(colnames(Pdata)[PSMFCcol]), "\n",
        "but only the first one will be used.")
    }
    PSMFCcol <- PSMFCcol[1]
  }
  

  # Use PSMFC column to find Puget Sound area
  out[grepl("4A", Pdata[, PSMFCcol], ignore.case = TRUE)] <- "Sound and Straits"

  # Find outside EEZ
  out[grepl("Z[yz]|9[gmyz]", Pdata[, PSMFCcol], ignore.case = TRUE)] <- "Outside EEZ"
  out[grepl("3[DN]", Pdata[, PSMFCcol], ignore.case = TRUE)] <- "CAN (VNCVR)"
  out[grepl("5[a-z]", Pdata[, PSMFCcol], ignore.case = TRUE)] <- "CAN"

  if (verbose) {
    message("The table below summarizes the number of records that are outside of\n",
      "the area that should be included for US West Coast stock assessments\n",
      "by PSMFC area, or some derivative thereof.\n",
      "Use Pdata[is.na(getArea(Pdata)), ] to filter for good areas.")
    capture.output(
      type = "message",
      table("PSMFC" = Pdata[, PSMFCcol], "CLEAN" = out)
      )
  }

  return(out)
}
