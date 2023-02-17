#' Pull Catch Data from PacFIN Database
#'
#' Pull Catch Data from the Comprehensive Fish Ticket table
#' in the PacFIN database.
#'
#' Species with special considerations:
#' URCK: There is an rockfish (URCK) category that consists on unassigned rockfish
#' landings. The majority of the catch is prior to 2001. Currently, there is no agreed upon
#' methodology to parsed these landing out to specific rockfish species. At present, landings
#' in this category are not included in species-specific rockfish catch pulls.
#' POP: In PacFIN there are four species code that can be associated with Pacific ocean perch.
#' These are POP, POP1, POP2, UPOP. The POP1 is general shelf/slope rockfish and not Pacific
#' ocean perch specific landings. These records occur only in Oregon. As of the 2017 assessment,
#' these records should be removed from the catch file.
#' 
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
#' 
#' 
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

  # Save pulled data
  catch.pacfin <- getDB(
    sql.catch(pacfin_species_code),
    username = username,
    password = password
  )
  savefn <- file.path(
    savedir,
    paste(
      "PacFIN", inputcode, "CompFT", format(Sys.Date(), "%d.%b.%Y"), "RData",
      sep = "."
    )
  )
  catch.pacfin <- data
  save(catch.pacfin, file = savefn)

  return(catch.pacfin)
}
