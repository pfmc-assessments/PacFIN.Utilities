#' Get length for PacFIN commercial samples
#'
#' Get length for PacFIN commercial samples in millimeters.
#' Lengths are converted for skates from type R and A to actual lengths.
#' Other, more standard lengths, are filtered for types in `keep`.
#'
#' @template Pdata
#' @template verbose
#' @template keep
#'
#' @export
#' @return A vector of lengths in millimeters. Values of `NA` indicate that
#' the length should not be used in down-stream calculations.
#'
getLength <- function(Pdata, verbose = FALSE, keep) {

  #### Initial checks
  # Can only accommodate good types
  goodtypes <- c("", "A", "D", "F", "R", "S", "T", "U", NA)
  if (any(!Pdata$FISH_LENGTH_TYPE %in% goodtypes)) {
    stop("cleanPacFIN can only accommodate the following FISH_LENGTH_TYPEs:\n",
      sprintf("'%s' ", goodtypes),
      "\nPlease contact the package maintainer to add additional types.")
  }

  # Find columns
  col.spid <- grep("SPID|PACFIN_SPECIES_CODE", colnames(Pdata), value = TRUE)[1]
  if (length(col.spid) != 1) {
    stop("Species ID column not found in Pdata.")
  }
  col.state <- grep("SOURCE_AGID|AGENCY_CODE", colnames(Pdata), value = TRUE)[1]
  if (length(col.state) != 1) {
    stop("State ID column not found in Pdata.")
  }

  # Check for "F" FISH_LENGTH_TYPE from California for spiny dogfish, a hack that
  # will eventually be removed (todo).
  check.calt <- which(
    Pdata[[col.spid]] == "DSRK" &
    Pdata[[col.state]] == "C" &
    Pdata[["FISH_LENGTH_TYPE"]] == "F"
    )
  if (length(check.calt) > 0) {
    message("Changing ", length(check.calt), " CA FISH_LENGTH_TYPE == 'F' to 'T'.",
      " Vlada is working on getting these entries fixed in PacFIN.")
    Pdata[check.calt, "FISH_LENGTH_TYPE"] <- "T"
  }
  rm(check.calt)

  # Move FISH_LENGTH to FORK_LENGTH if FORK_LENGTH is NA and type is F
  # for downstream code to work
  Pdata[, "FORK_LENGTH"] <- ifelse(
    is.na(Pdata[, "FORK_LENGTH"]) & Pdata[, "FISH_LENGTH_TYPE"] == "F",
    Pdata[, "FISH_LENGTH"],
    Pdata[, "FORK_LENGTH"]
    )

  #### Species-specific code
  # Convert FISH_LENGTH from disk width to length
  width2length <- convertlength_skate(Pdata, returntype = "estimated")

  # Spiny dogfish (Squalus suckleyi; DSRK)
  check.dogfish <- Pdata[[col.spid]] == "DSRK" & !is.na(Pdata[["FORK_LENGTH"]])
  if (sum(check.dogfish) > 0 & verbose) {
    message(sum(check.dogfish), " fork lengths were converted to total lengths using\n",
      "Tribuzio and Kruse (2012).")
  }
  Pdata[check.dogfish, "FORK_LENGTH"] <-
    ifelse(Pdata[check.dogfish, "FISH_LENGTH_UNITS"] == "MM", 12.2, 1.22) +
    1.07 * Pdata[check.dogfish, "FORK_LENGTH"]

  # Fix incorrect FISH_LENGTH_UNITS for hake
  if (length(grep("PWHT", Pdata[["SPID"]])) > 0) {
    if (verbose) message("Still fixing WA FISH_LENGTH_UNITS")
    Pdata[, "FISH_LENGTH_UNITS"] <- ifelse(
      tolower(Pdata[, "FISH_LENGTH_UNITS"]) == "cm" & Pdata[, "FISH_LENGTH"] > 90,
      "MM",
      Pdata[, "FISH_LENGTH_UNITS"]
      )
  }

  #### Make "length" column in mm
  # Start with fork lengths for those that are available and if "F" in keep
  Pdata$length <- ifelse(Pdata$FISH_LENGTH_TYPE %in% c("", "A", "F", NA),
    Pdata$FORK_LENGTH, NA)

  # Work with skate data
  # A is disc width
  # R is inter-spiracle width for skates (used by WDFW)
  if (all(Pdata$SPID %in% c("LSKT", "BSKT"))) {
    Pdata$length <- ifelse(
      "A" %in% keep & Pdata$FISH_LENGTH_TYPE == "A",
      width2length,
      Pdata$length)
  }
  Pdata$length <- ifelse(
    "R" %in% keep & Pdata$FISH_LENGTH_TYPE == "R",
    width2length,
    Pdata$length)

  # Work with dorsal length
  if (
    verbose &
    "D" %in% keep &
    length(grep("D", Pdata[["FISH_LENGTH_TYPE"]]) > 0)
    ) {
    message("Using dorsal lengths, are you sure you want dorsal lengths?")
  }
  Pdata$length <- ifelse(
    "D" %in% keep & Pdata$FISH_LENGTH_TYPE == "D" &
    Pdata$FORK_LENGTH != Pdata$FISH_LENGTH,
      Pdata$FORK_LENGTH, Pdata$length)

  # Work with standard length measurements and unknown type
  Pdata$length <- ifelse(
    "S" %in% keep & Pdata$FISH_LENGTH_TYPE == "S",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    "T" %in% keep & Pdata$FISH_LENGTH_TYPE == "T",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    "U" %in% keep & Pdata$FISH_LENGTH_TYPE == "U",
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length)
  Pdata$length <- ifelse(
    "" %in% keep & Pdata$FISH_LENGTH_TYPE == "",
    Pdata$FISH_LENGTH,
    Pdata$length)
  Pdata$length <- ifelse(
    NA %in% keep & is.na(Pdata$FISH_LENGTH_TYPE),
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length)

  # A double check that lengths for methods not in keep are NA
  Pdata$length <- ifelse(
    Pdata$FISH_LENGTH_TYPE %in% keep,
    Pdata$length,
    NA)

  # Assign all fish of length zero to NA
  if (verbose & any(Pdata[["length"]] == 0, na.rm = TRUE)) {
    message(sum(Pdata[["length"]] == 0),
      " fish had a length equal to 0 (mm) and were assigned a length of NA.")
  }
  Pdata$length[Pdata$length == 0] <- NA

  # Ensure everything is in mm
  if ("FISH_LENGTH_UNITS" %in% colnames(Pdata)) {
    Pdata$length <- ifelse(
      tolower(Pdata[, "FISH_LENGTH_UNITS"]) == "cm",
      Pdata[, "length"] * 10,
      Pdata[, "length"])
  } else {
    if (verbose) message("Length assumed to be in mm.")
  }

  if (verbose) {
    message("\nThe following length types were kept in the data:")
    capture.output(type = "message",
      table(output = Pdata[
        !is.na(Pdata[["length"]]),
        grep("LENGTH_TYPE", colnames(Pdata))
        ])
      )
    message(
      "Lengths range from ",
      paste(collapse = " to ", range(Pdata[["length"]], na.rm = TRUE)),
      " (mm)."
      )
  }
  return(Pdata[["length"]])
}
