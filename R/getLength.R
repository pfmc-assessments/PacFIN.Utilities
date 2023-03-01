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
getLength <- function(Pdata, verbose = TRUE, keep) {

  # Initial checks
  # Early return
  if (all(is.na(Pdata[["FISH_LENGTH"]]))) {
    if (verbose) {
      cli::cli_alert_success("No lengths were found, moving on.")
    }
    return(rep(NA, NROW(Pdata)))
  }
  # Stop
  data_with_length <- dplyr::filter(
    .data = Pdata,
    !is.na(FISH_LENGTH),
    !(FISH_LENGTH_UNITS %in% c("MM", "CM"))
  )
  if (NROW(data_with_length)) {
    cli::cli_abort(
      glue::glue(
        "FISH_LENGTH_UNITS contains units other than 'CM' or 'MM' for fish",
        " with lengths, please assign a unit like 'CM' or 'MM' to each row."
      ),
      wrap = TRUE,
      call = NULL
    )
  }

  # Find columns
  var_spid <- grep(
    pattern = "^SPID$|PACFIN_SPECIES_CODE",
    x = colnames(Pdata),
    value = TRUE
  )[1]
  var_state <- grep("SOURCE_AGID|AGENCY_CODE", colnames(Pdata), value = TRUE)[1]
  var_fish_length_type <- grep(
    pattern = "^FISH_LENGTH_TYPE$|^FISH_LENGTH_TYPE_CODE",
    x = colnames(Pdata),
    value = TRUE
  )

  # Can only accommodate good types
  good_types <- c("", "A", "D", "F", "R", "S", "T", "U", NA)
  good_types_string <- glue::glue_collapse(sQuote(good_types), sep = ", ")
  if (any(!Pdata[[var_fish_length_type]] %in% good_types)) {
    cli::cli_abort(
      glue::glue(
        "getLength can only accommodate the following FISH_LENGTH_TYPEs,
        please contact the package maintainer to add additional types:
        {good_types_string}
        "
      ),
      call = NULL
    )
  }

  # Check for "F" FISH_LENGTH_TYPE from California for spiny dogfish, a hack that
  # will eventually be removed (todo).
  check.calt <- which(
    Pdata[[var_spid]] == "DSRK" &
    Pdata[[var_state]] == "C" &
    Pdata[[var_fish_length_type]] == "F"
  )
  if (length(check.calt) > 0) {
    message("Changing ", length(check.calt), " CA FISH_LENGTH_TYPE == 'F' to 'T'.",
      " Vlada is working on getting these entries fixed in PacFIN.")
    Pdata[check.calt, var_fish_length_type] <- "T"
  }
  rm(check.calt)

  # Move FISH_LENGTH to FORK_LENGTH if FORK_LENGTH is NA and type is F
  # for downstream code to work

  Pdata[, "FORK_LENGTH"] <- ifelse(
    is.na(Pdata[["FORK_LENGTH"]]) & Pdata[[var_fish_length_type]] == "F",
    yes = Pdata[, "FISH_LENGTH"],
    no = Pdata[, "FORK_LENGTH"]
  )

  # Species-specific code
  # Convert FISH_LENGTH from disk width to length
  width2length <- convertlength_skate(Pdata, returntype = "estimated")

  # Spiny dogfish (Squalus suckleyi; DSRK)
  check.dogfish <- Pdata[[var_spid]] == "DSRK" & !is.na(Pdata[["FORK_LENGTH"]])
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

  # Make "length" column in mm
  # Start with fork lengths for those that are available and if "F" in keep
  Pdata$length <- ifelse(
    Pdata[[var_fish_length_type]] %in% c("", "A", "F", NA),
    Pdata$FORK_LENGTH,
    NA
  )

  # Work with skate data
  # A is disc width
  # R is inter-spiracle width for skates (used by WDFW)
  if (all(Pdata$SPID %in% c("LSKT", "BSKT"))) {
    Pdata$length <- ifelse(
      "A" %in% keep & Pdata[[var_fish_length_type]] == "A",
      width2length,
      Pdata$length
    )
  }
  Pdata$length <- ifelse(
    "R" %in% keep & Pdata[[var_fish_length_type]] == "R",
    width2length,
    Pdata$length
  )

  # Work with dorsal length
  if (
    verbose &
    "D" %in% keep &
    length(grep("D", Pdata[[var_fish_length_type]]) > 0)
    ) {
    message("Using dorsal lengths, are you sure you want dorsal lengths?")
  }
  Pdata$length <- ifelse(
    "D" %in% keep & Pdata[[var_fish_length_type]] == "D" &
    Pdata$FORK_LENGTH != Pdata$FISH_LENGTH,
    Pdata$FORK_LENGTH, Pdata$length
  )

  # Work with standard length measurements and unknown type
  Pdata$length <- ifelse(
    "S" %in% keep & Pdata[[var_fish_length_type]] == "S",
    Pdata$FISH_LENGTH,
    Pdata$length
  )
  Pdata$length <- ifelse(
    "T" %in% keep & Pdata[[var_fish_length_type]] == "T",
    Pdata$FISH_LENGTH,
    Pdata$length
  )
  Pdata$length <- ifelse(
    "U" %in% keep & Pdata[[var_fish_length_type]] == "U",
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length
  )
  Pdata$length <- ifelse(
    "" %in% keep & Pdata[[var_fish_length_type]] == "",
    Pdata$FISH_LENGTH,
    Pdata$length
  )
  Pdata$length <- ifelse(
    NA %in% keep & is.na(Pdata[[var_fish_length_type]]),
    ifelse(is.na(Pdata$FORK_LENGTH), Pdata$FISH_LENGTH, Pdata$FORK_LENGTH),
    Pdata$length
  )

  # A double check that lengths for methods not in keep are NA
  Pdata$length <- ifelse(
    Pdata[[var_fish_length_type]] %in% keep,
    Pdata$length,
    NA
  )

  # Assign all fish of length zero to NA
  i_length_0 <- Pdata[["length"]] == 0
  Pdata$length[i_length_0] <- NA

  # Ensure everything is in mm
  # As of 2023-02-28 there are only two valid units in PacFIN for length
  # MM and CM, everything else is NULL or UNK
  Pdata$length <- ifelse(
    test = Pdata[, "FISH_LENGTH_UNITS"] == "CM",
    yes = Pdata[, "length"] * 10,
    no = Pdata[, "length"]
  )


  if (verbose) {
    cli::cli_alert_info(paste(
      "The following length types were kept in the data:",
      sQuote(unique(Pdata[!is.na(Pdata[["length"]]), var_fish_length_type]))
    ))
    cli::cli_alert_info(glue::glue(
      "Lengths ranged from {min(Pdata[['length']], na.rm = TRUE)}--",
      "{max(Pdata[['length']], na.rm = TRUE)} (mm)"
    ))
    cli::cli_alert_info(glue::glue(
      sum(Pdata[["length"]] == 0, na.rm = TRUE),
      " fish had lengths of 0 (mm) and were changed to NAs"
    ))
    cli::cli_alert_info(glue::glue(
      "{sum(Pdata[['FISH_LENGTH_UNITS']] == 'CM', na.rm = TRUE)}",
      " lengths (cm) and were converted to mm"
    ))
  }
  return(Pdata[["length"]])
}
