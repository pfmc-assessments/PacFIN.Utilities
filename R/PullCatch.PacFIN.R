#' Pull Catch Data from PacFIN Database
#'
#' Pull Catch Data from the Comprehensive Fish Ticket table
#' in the PacFIN database.
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @template savedir
#' @param addnominal A logical, the default \code{TRUE} adds the nominal
#' PacFIN species code to the list of those searched for. \code{FALSE}
#' leaves \code{pacfin_species_code} as input by the user which will miss
#' catch for species such as dover sole that have more than one species
#' code within PacFIN, e.g., \code{pacfin_species_code = c("DOVR", "DVR1")}.
#' Users can also input a vector to pacfin_species_code if you want to
#' specify the species and the nominal species code without searching for
#' it. This is helpful for when you are getting data for two species and
#' you only want nominal catch for one or if you only wanted nominal catch.
#' Nominal species code will be searched for using [PullNominal.PacFIN] if
#' the input value for `addnominal` is `TRUE`.
#' @template verbose
#'
#' @export
#' @author Kelli F. Johnson
#' @seealso [PullNominal.PacFIN] for how nominal species codes are determined.
#' @return RData frames are saved to the disk and the pulled data
#' are returned as a data frame.
#'   * CompFT - pulled data
#'   * Research.Tribal.Catch - summary by year, state, and fleet;
#'     fleets are as follows:
#'     limited entry (LE), open access (OA), treaty indian (TI),
#'     research (R), and unknown (XX)
#'   * Catch.INPFC - catch by INPFC area
#'   * Catch.PSMFC - catch by PSMFC area
#'
PullCatch.PacFIN <- function(pacfin_species_code,
                             username = getUserName("PacFIN"),
                             password,
                             savedir = getwd(),
                             addnominal = TRUE,
                             verbose = FALSE) {

  inputcode <- pacfin_species_code

  #### Pull from PacFIN
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes\n")
  }

  # Find nominal code if there is one and the user hasn't passed it
  if (addnominal[1] == TRUE) {
    thenominal <- PullNominal.PacFIN(
      pacfin_species_code = pacfin_species_code,
      username = username,
      password = password
    )
    pacfin_species_code <- c(pacfin_species_code, stats::na.omit(thenominal))
    if (verbose) {
      message(
        "The following nominal species codes were added: ",
        knitr::combine_words(thenominal)
      )
    }
  } else { # Add the nominal code passed by the user unless FALSE
    if (addnominal[1] != FALSE) {
      pacfin_species_code <- c(pacfin_species_code, addnominal)
    }
  }
  data <- getDB(
    sql.catch(pacfin_species_code),
    username = username,
    password = password
  )

  #### Create summaries
  data_fleet <- stats::aggregate(
    list(
      ROUND_WEIGHT_LBS = data[["ROUND_WEIGHT_LBS"]],
      ROUND_WEIGHT_MTONS = data[["ROUND_WEIGHT_MTONS"]]),
    data[, c("LANDING_YEAR", "FLEET_CODE", "AGENCY_CODE")],
    sum, na.rm = TRUE)
  data_woR <- data[data[["FLEET_CODE"]] != "R", ]
  data_inpfc <- stats::aggregate(
    list(
      ROUND_WEIGHT_LBS = data_woR[["ROUND_WEIGHT_LBS"]],
      ROUND_WEIGHT_MTONS = data_woR[["ROUND_WEIGHT_MTONS"]]),
    data_woR[, c("COUNCIL_CODE", "DAHL_GROUNDFISH_CODE", "PACFIN_SPECIES_CODE",
      "PACFIN_GROUP_GEAR_CODE", "PACFIN_PORT_CODE", "PACFIN_GEAR_CODE",
      "INPFC_AREA_TYPE_CODE", "LANDING_MONTH", "LANDING_YEAR", "AGENCY_CODE")],
    sum, na.rm = TRUE)
  data_psmfc <- stats::aggregate(
    list(
      ROUND_WEIGHT_LBS = data_woR[["ROUND_WEIGHT_LBS"]],
      ROUND_WEIGHT_MTONS = data_woR[["ROUND_WEIGHT_MTONS"]]
    ),
    data_woR[, c("COUNCIL_CODE", "DAHL_GROUNDFISH_CODE", "PACFIN_SPECIES_CODE",
      "PACFIN_GROUP_GEAR_CODE", "PACFIN_PORT_CODE", "PACFIN_GEAR_CODE",
      "PACFIN_CATCH_AREA_CODE", "LANDING_MONTH", "LANDING_YEAR",
      "AGENCY_CODE")],
    FUN = sum,
    na.rm = TRUE
  )

  #### Save appropriate summaries
  savefn <- file.path(
    savedir,
    paste(
      "PacFIN", inputcode, "CompFT", format(Sys.Date(), "%d.%b.%Y"), "RData",
      sep = "."
    )
  )
  catch.pacfin <- data
  save(catch.pacfin, file = savefn)

  return(data)
}
