#' Pull catch data from PacFIN
#'
#' Pull catch data from the Comprehensive Fish Ticket table in PacFIN.
#'
#' @details
#' ## Species with special considerations
#' ### URCK
#' There is a rockfish (URCK) category that consists of unassigned rockfish
#' landings. The majority of the catch is prior to 2001. Currently, there is no
#' agreed upon methodology to parsed these landing out to specific rockfish
#' species. At present, landings in this category are not included in
#' species-specific rockfish catch pulls.
#' ### POP
#' In PacFIN there are four species code that can be associated with Pacific
#' ocean perch. These are POP, POP1, POP2, UPOP. The POP1 is general shelf/slope
#' rockfish and not Pacific ocean perch specific landings. These records occur
#' only in Oregon. As of the 2017 assessment, these records should be removed
#' from the catch file.
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @template savedir
#' @param addnominal A logical, where the default, `TRUE`, adds nominal
#' PacFIN species code to `pacfin_species_code`. `FALSE`
#' leaves `pacfin_species_code` as input by the user, which will miss
#' catch for species such as dover sole that have more than one species
#' code within PacFIN, e.g., `pacfin_species_code = c("DOVR", "DVR1")`.
#' Users can also input a vector to pacfin_species_code if you want to
#' specify the species and the nominal species code without searching for
#' it. This is helpful for when you are getting data for two species and
#' you only want nominal catch for one or if you only wanted nominal catch.
#' Nominal species code will be searched for using [PullNominal.PacFIN()] if
#' the input value for `addnominal` is `TRUE`.
#' @template verbose
#'
#' @export
#' @author Kelli F. Johnson
#' @seealso
#' See [PullNominal.PacFIN()] for how nominal species codes are determined.
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
#' 
#' 
#'
PullCatch.PacFIN <- function(pacfin_species_code,
                             username = getUserName("PacFIN"),
                             password = ask_password(),
                             savedir = getwd(),
                             addnominal = TRUE,
                             verbose = FALSE) {
  # Input checks
  stopifnot(
    "`addnominal` must be a logical." =
    is.logical(addnominal) &&
    length(addnominal) == 1
  )
  stopifnot(
    "`verbose` must be a logical." =
    is.logical(verbose) &&
    length(verbose) == 1
  )
  file_species_code <- paste(pacfin_species_code, collapse = "--")

  # Find nominal codes if they exist beyond those provided in
  # pacfin_species_code
  if (addnominal) {
    pacfin_nominal_code <- PullNominal.PacFIN(
      pacfin_species_code = pacfin_species_code,
      username = username,
      password = password
    )
    pacfin_species_code <- c(
      pacfin_species_code,
      stats::na.omit(pacfin_nominal_code)
    )
    if (verbose && length(stats::na.omit(pacfin_nominal_code))) {
      message(
        "The following nominal species codes were added: ",
        glue::glue_collapse(pacfin_nominal_code, sep = ", ", last = " and ")
      )
    }
  }

  # Save pulled data
  catch.pacfin <- getDB(
    sql.catch(pacfin_species_code),
    username = username,
    password = password
  )
  savefn <- file.path(
    savedir,
    paste(
      "PacFIN",
      file_species_code,
      "CompFT",
      format(Sys.Date(), "%d.%b.%Y"),
      "RData",
      sep = "."
    )
  )
  save(catch.pacfin, file = savefn)

  return(invisible(catch.pacfin))
}
