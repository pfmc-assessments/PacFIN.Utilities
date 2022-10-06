#' Create column for gear called `geargroup` according to PacFIN gears
#'
#' Data from the PacFIN
#' [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in \code{Pdata} called \code{geargroup}, where
#' \code{Pdata$GRID} is recoded to \code{geargroup} according to the gear table.
#'
#' @template Pdata
#' @template spp
#' @template verbose
#'
#' @export
#' @seealso Reverse dependency of [cleanPacFIN]
#'
#' @return A modified data frame where an additional column labeled
#'   \code{geargroup} is added to \code{Pdata}. No original columns are modified
#'   in the process.
#' @author Andi Stephens
#' @examples
#' ex <- getGearGroup(data.frame(GRID = c("PRT", "BMT", "FPT")), verbose = FALSE)
#' table(ex)
#' testthat::expect_equal(ex[ex[, "geargroup"] == "POT", "GRID"], "FPT")
#'
getGearGroup = function (Pdata, spp = NULL, verbose = TRUE) {

  #### Checks
  if (verbose) {
    message("\nGear groupings reflect those in the table at\n",
       "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt")
  }
  if (!"GRID" %in% colnames(Pdata)) {
    if ("PACFIN_GEAR_CODE" %in% colnames(Pdata)) {
      Pdata[, "GRID"] <- Pdata[, "PACFIN_GEAR_CODE"]
    } else {
    stop("Pdata must have 'GRID' or 'PACFIN_GEAR_CODE' as a column.")
    }
  }
  if (is.factor(Pdata[, "GRID"])) {
    Pdata[, "GRID"] <- as.character(Pdata[, "GRID"])
  }

  #### Species-specific code to alter the PacFIN gear table
  if (!is.null(spp)) {
    if (tolower(spp) %in% c("sablefish", "sabl")) {
      msc <- Pdata[Pdata[, "GRID"] %in% c("DNT", "LJ", "JIG", "MDT", "MPT"), ]
      if (nrow(msc) > 0) {
        if (verbose) {
          message("The following samples were assigned to the gear group 'MSC':")
          utils::write.table(table(msc[, "GRID"]),
            col.names = FALSE, row.names = FALSE)
        }
        # Danish/Scottish Seine trawl
        GearTable[, "GROUP"][GearTable$GRID == "DNT"] <- "MSC"
        # Midwater trawl
        GearTable[, "GROUP"][GearTable$GRID %in% c("MDT", "MPT")] <- "MSC"
        # Handline jigger
        GearTable[, "GROUP"][GearTable$GRID == "JIG"] <- "MSC"
      }
    } # end if spp == sablefish
    if (any(grepl("dogfish|dsrk", spp, ignore.case = TRUE))) {
      if (verbose) {
        message("Dogfish uses a mid-water trawl (MID), TWL (including shrimp), and HKL fleets\n",
          "everything else is assigned to MSC.")
      }
      GearTable[grepl("MIDWATER", GearTable[["DESCRIPTION"]]), "GROUP"] <- "MID"
      GearTable[grepl("DRG|NET|NTW|POT|TLS", GearTable[["GROUP"]]), "GROUP"] <- "MSC"
      GearTable[grepl("TWS", GearTable[["GROUP"]]), "GROUP"] <- "TWL"
    } # end if spp == dogfish
  }

  #### Create geargroup
  Pdata[, "geargroup"] <- GearTable[match(Pdata[, "GRID"], GearTable[, "GRID"]), "GROUP"]
  Pdata[, "geargroup"] <- ifelse(is.na(Pdata[, "geargroup"]),
    Pdata[, "GRID"], Pdata[, "geargroup"])

  if (verbose) {
    message("GRID was assigned to geargroup with the following names:")
    utils::capture.output(type = "message", table(Pdata[, "geargroup"]))
  }

  return(Pdata)

}
