#' @param pacfin_species_code A vector of strings specifying the PacFIN species
#'   code(s) you are interested in. This has sometimes been referred to as
#'   `"SPID"` in legacy sql scripts. An example for sablefish would be
#'   `pacfin_species_code = "SABL"`. Lists of species codes in [hierarchical
#'   order](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp_tree.txt),
#'   by
#'   [organization](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_spcodes.txt),
#'   and [alphabetically
#'   organized](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_spcodes_spid.txt)
#'   can be found on the PacFIN website](https://pacfin.psmfc.org). Often you
#'   will want to include nominal species categories. Where, nominal (i.e.,
#'   [existing in name
#'   only](https://www.google.com/search?q=NOMINAL&rlz=1C1GCEJ_enUS866US866&sourceid=chrome&ie=UTF-8))
#'   means information for a given species that is `derived' from non-species
#'   specific information, e.g., species complexes that are split out by species
#'   compositions like `nominal aurora rockfish' which would be `ARR1`. For some
#'   functions, these nominal categories can automatically be added, see the
#'   argument `addnominal`.
