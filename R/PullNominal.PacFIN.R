#' Pull the species information table and return nominal code
#'
#' Pull the species information table with PACFIN_SPECIES_CODE and
#' PACFIN_COMMON_NAME from PacFIN to determine the nominal code for
#' a given species code.
#'
#' @template pacfin_species_code
#' @template username
#' @template password
#' @return A vector of character values, where each value is a
#' PACFIN_SPECIES_CODE that pertains to the input `pacfin_species_code`.
#' `NA` is returned if no values are found.
#'
PullNominal.PacFIN <- function(pacfin_species_code, username, password) {

  spp <- getDB(
    sql.species(),
    username = username,
    password = password
  )

  nom <- spp[grepl("NOM\\.", spp[,2]), ]

  thenominal <- tibble::tibble(spp) %>%
    dplyr::mutate(searchname = gsub(
      "BLACK AND YELLOW",
      "BLACK-AND-YELLOW",
      PACFIN_SPECIES_COMMON_NAME
      )) %>%
    dplyr::mutate(searchname = gsub(
      "CALIFORNIA HALIBUT",
      "CALIF HALIBUT",
      searchname
      )) %>%
    dplyr::mutate(searchname = gsub(
      "PACIFIC OCEAN PERCH",
      "POP",
      searchname
      )) %>%
    dplyr::mutate(searchname = gsub(
      "(CHILIPEPPER|SQUARESPOT|VERMILION) ROCKFISH",
      "\\1",
      searchname
      )) %>%
    dplyr::mutate(nominal = purrr::map_chr(searchname,
      ~paste0(grep(.x, nom[, 2], value = TRUE), collapse = "|"))) %>%
    dplyr::mutate(code = purrr::map(nominal, ~{
      if (.x[1] == "") return(NA)
      return(nom[grep(.x, nom[, 2]), 1])
    })) %>%
    dplyr::filter(PACFIN_SPECIES_CODE == pacfin_species_code) %>%
    dplyr::pull(code) %>%
    unlist()

  return(thenominal)

}
