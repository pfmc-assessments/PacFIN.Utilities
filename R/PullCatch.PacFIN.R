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
#' @export
#' @author John R. Wallace, Kelli Faye Johnson
#' @return RData frames are saved to the disk and the pulled data
#' are returned as a data frame.
#' \list{
#'   \item{CompFT - pulled data}
#'   \item{Research.Tribal.Catch - summary by year, state, and fleet; 
#'     fleets are as follows:
#'     limited entry (LE), open access (OA), treaty indian (TI),
#'     research (R), and unknown (XX)}
#'   \item{Catch.INPFC - catch by INPFC area}
#'   \item{Catch.PSMFC - catch by PSMFC area}
#' }
#'
PullCatch.PacFIN <- function(pacfin_species_code,
  username = getUserName("PacFIN"), password, savedir = getwd(),
  addnominal = TRUE) {

  #### Pull from PacFIN
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes\n")
  }
  ar <- getDB(sql.area(),
    username = username, password = password)
  # Find nominal code if there is one and the user hasn't passed it
  if (addnominal) {
    spp <- getDB(sql.species(),
      username = username, password = password)
    pacfin_species_code <- spp[
      grepl(paste0(collapse = "|", gsub("NOM. ", "",
        spp[
          match(pacfin_species_code, spp[["PACFIN_SPECIES_CODE"]]),
          "PACFIN_SPECIES_COMMON_NAME"])),
        spp[, "PACFIN_SPECIES_COMMON_NAME"]
      ), "PACFIN_SPECIES_CODE"]
  }
  data <- getDB(sql.catch(pacfin_species_code),
    username = username, password = password)
  data <- cleanColumns(data)

  #### Create summaries
  data.fleet <- stats::aggregate(
    list(
      Catch.mt = data[["CATCH.LBS"]] / 2204.62,
      ROUND_WEIGHT_MTONS = data[["ROUND_WEIGHT_MTONS"]]),
    data[, c("YEAR", "FLEET", "AGID")],
    sum, na.rm = TRUE)
  data.woR <- data[data[["FLEET"]] != "R", ]
  data.inpfc <- stats::aggregate(
    list(
      CATCH.KG = data.woR[["CATCH.LBS"]] / 2.20462,
      ROUND_WEIGHT_MTONS = data.woR[["ROUND_WEIGHT_MTONS"]]),
    data.woR[, c("COUNCIL", "DAHL_SECTOR", "SPID",
      "GRGROUP", "PCID", "GRID", "INPFC_ARID", "PERIOD", "YEAR", "AGID")],
    sum, na.rm = TRUE)
  data.psmfc <- stats::aggregate(
    list(
      CATCH.KG = data.woR[["CATCH.LBS"]] / 2.20462,
      ROUND_WEIGHT_MTONS = data.woR[["ROUND_WEIGHT_MTONS"]]),
    data.woR[, c("COUNCIL", "DAHL_SECTOR", "SPID",
      "GRGROUP", "PCID", "GRID", "ARID", "PERIOD", "YEAR", "AGID")],
    sum, na.rm = TRUE)

  #### Save appropriate summaries
  savefn <- file.path(savedir, paste(sep = ".",
    pacfin_species_code[1], "CompFT",
    format(Sys.Date(), "%d.%b.%Y"),
    "RData"))
  save(data, file = savefn)
  save(data.fleet, file = gsub("CompFT", "Research.Tribal.Catch", savefn))
  save(data.inpfc, file = gsub("([A-Z]+)[.]CompFT", "PacFIN.\\1.Catch.INPFC", savefn))
  save(data.psmfc, file = gsub("([A-Z]+)[.]CompFT", "PacFIN.\\1.Catch.PSMFC", savefn))

  return(data)
}
