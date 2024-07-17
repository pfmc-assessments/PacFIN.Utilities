#' Pull catch data from PacFIN
#'
#' Pull catch data from the Comprehensive Fish Ticket table in PacFIN.
#'
#' @inheritParams sql
#' @inheritParams getDB
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
#' @return
#' A `.RData` file is saved with the object inside the file stored as
#' `catch.pacfin`. This same data frame is also returned invisibly.
#'
#' @details
#' ## Species with special considerations
#'
#' ### URCK
#' There is a rockfish (URCK) category that consists of unassigned rockfish
#' landings. The majority of the catch is prior to 2001. Currently, there is no
#' agreed upon methodology to parsed these landing out to specific rockfish
#' species. At present, landings in this category are not included in
#' species-specific rockfish catch pulls.
#'
#' ### POP
#' In PacFIN there are four species code that can be associated with Pacific
#' ocean perch. These are POP, POP1, POP2, UPOP. The POP1 is general shelf/slope
#' rockfish and not Pacific ocean perch specific landings. These records occur
#' only in Oregon. As of the 2017 assessment, these records should be removed
#' from the catch file.
#'
#' ## Searching for species
#' Values passed to `pacfin_species_code` are searched for in their exact form.
#' That is, there are no regular expression searches so mistakes such as
#' `" POP"` will not be found. In my experiences these mistakes in the species
#' codes are more common for PacFIN species codes that are three letters rather
#' than the standard four letters.
#'
#' @export
#' @author Kelli F. Johnson
#'
#' @seealso
#' * [PullNominal.PacFIN()] determines the nominal species codes
#' * [sql_catch()] writes the sql code to pull the data
#'
#' @examples
#' \dontrun{
#' catch.pacfin <- PullCatch.PacFIN("PTRL")
#' # Check for confidentiality by year
#' # though you would also want to do this by your gear types
#' dplyr::group_by(catch.pacfin, LANDING_YEAR) %>%
#'   dplyr::summarize(count = dplyr::n_distinct(VESSEL_ID)) %>%
#'   dplyr::filter(count < 4)
#'
#' # look for foreign landings
#' catch.pacfin <- PullCatch.PacFIN("PTRL", council_code = c("*", "N"))
#' # Counts of NROW() by area code(s)
#' dplyr::group_by(catch.pacfin, ORIG_PACFIN_CATCH_AREA_CODE) %>%
#'   dplyr::count()
#' }
#'
PullCatch.PacFIN <- function(pacfin_species_code,
                             council_code = "P",
                             username = getUserName("PacFIN"),
                             password = ask_password(),
                             savedir = getwd(),
                             addnominal = TRUE,
                             verbose = TRUE) {
  # todo:
  # * change input arguments to snake_case
  # * get rid of addnominal call and show users how to add it in pacfin_species_code

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

  catch.pacfin <- getDB(
    sql_catch(pacfin_species_code, council_code),
    username = username,
    password = password
  )

  # message calls
  if (verbose) {
    message(
      "\nThe following PACFIN_SPECIES_CODE(s) were found:\n",
      paste0(
        utils::capture.output(
          dplyr::count(catch.pacfin, PACFIN_SPECIES_CODE) %>%
            dplyr::mutate(PACFIN_SPECIES_CODE = sQuote(PACFIN_SPECIES_CODE))
        ),
        collapse = "\n"
      ),
      "\n"
    )
  }

  # Save pulled data
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
