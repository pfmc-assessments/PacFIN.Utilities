#' Create column for gear called `geargroup` according to PacFIN gears
#'
#' Data from the PacFIN [gear
#' table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in `Pdata` called `geargroup`, where
#' `Pdata[["GRID"]] is recoded to `geargroup` according to the gear group listed
#' in the table.
#'
#' @details
#' For some species the recoding is more intense. For example, sablefish
#' `GROUPS` are recoded into just three gears, hook and line (HKL), pot (POT),
#' and trawl (TWL). This is done internally to maintain consistency across
#' assessment authors. Other species can easily be added to this function. If a
#' species name is not passed then no additional recoding is performed compared
#' to the matching available in the table pulled from online.
#'
#' @param Pdata A data frame, typically one extracted from PacFIN but for this
#'   function just one column is needed, `GRID`. If a newer data pull is used,
#'   the function will change `PACFIN_GEAR_CODE` to `GRID` for you.
#' @inheritParams cleanPacFIN
#'
#' @export
#' @seealso
#' * [cleanPacFIN()] uses this function
#'
#' @return
#' A modified data frame where an additional column labeled \code{geargroup} is
#' added to \code{Pdata}. No original columns are modified in the process.
#' @author Andi Stephens
#' @examples
#' gears <- c("PRT", "FPT", "TWL", "MDP")
#' X <- getGearGroup(data.frame(GRID = gears), verbose = TRUE)
#' table(X[["geargroup"]])
#'
getGearGroup <- function(Pdata,
                         spp = NULL,
                         verbose = TRUE) {
  # Checks
  if (verbose) {
    cli::cli_bullets(c(
      "i" = "Gear groupings reflect those in the table at",
      "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt"
    ))
  }
  if (!"GRID" %in% colnames(Pdata)) {
    if ("PACFIN_GEAR_CODE" %in% colnames(Pdata)) {
      Pdata[, "GRID"] <- Pdata[, "PACFIN_GEAR_CODE"]
    } else {
      cli::cli_abort("Pdata must have 'GRID' or 'PACFIN_GEAR_CODE' column.")
    }
  }
  if (is.factor(Pdata[, "GRID"])) {
    Pdata[, "GRID"] <- as.character(Pdata[, "GRID"])
  }

  # Species-specific code to alter the PacFIN gear table
  if (!is.null(spp)) {
    if (tolower(spp) %in% c("sablefish", "sabl")) {
      change_to_trawl <- c(
        # Midwater trawl, diving (b/c there is so little), other trawls, dredge
        "DGN", "DPN", "DVG", "DRG", "GLN", "MDT", "MPT", "MSC", "ODG", "ONT",
        "OTH", "PWT", "RVT", "SCD", "SEN", "STN", "TWS", "USP",
        # all nets
        "BTR", "DST", "NET", "SGN", "SHT", "SST", "TML", "TRL"
      )
      change_to_hkl <- c("HTR", "NTW", "PTR", "TLS")
      if (verbose) {
        cli::cli_bullets(
          c(
            "i" = "sablefish uses HKL, POT, and TWL",
            "i" = "{change_to_hkl} are recoded to HKL.",
            "i" = "almost everything outside of the above is recoded to TWL."
          )
        )
      }
      GearTable[, "GROUP"][GearTable$GRID %in% change_to_trawl] <- "TWL"
      # Trolling gear and non-trawl
      GearTable[, "GROUP"][
        GearTable$GRID %in% change_to_hkl
      ] <- "HKL"
    } # end if spp == sablefish
    if (any(grepl("dogfish|dsrk", spp, ignore.case = TRUE))) {
      if (verbose) {
        cli::cli_bullets(c(
          "i" = "Dogfish uses a mid-water trawl (MID), TWL (including shrimp),
                 and HKL fleets",
          "i" = "everything else is assigned to MSC."
        ))
      }
      GearTable[grepl("MIDWATER", GearTable[["DESCRIPTION"]]), "GROUP"] <- "MID"
      GearTable[grepl("TWS", GearTable[["GROUP"]]), "GROUP"] <- "TWL"
      # Assign everything else to MSC
      GearTable[!GearTable[["GROUP"]] %in% c("MID", "TWL"), "GROUP"] <- "MSC"
    } # end if spp == dogfish
  }

  #### Create geargroup
  Pdata[, "geargroup"] <- GearTable[
    match(Pdata[, "GRID"], GearTable[, "GRID"]),
    "GROUP"
  ]
  Pdata[, "geargroup"] <- ifelse(
    test = is.na(Pdata[, "geargroup"]),
    Pdata[, "GRID"],
    Pdata[, "geargroup"]
  )

  if (verbose) {
    table_gears <- table(Pdata[, "geargroup"])
    message_table <- paste0(
      names(table_gears),
      " (", table_gears, ")"
    )
    cli::cli_bullets(c(
      "i" = "`GRID` was used to create `geargroup`",
      "i" = "geargroup includes: {message_table}"
    ))
  }

  return(Pdata)
}
