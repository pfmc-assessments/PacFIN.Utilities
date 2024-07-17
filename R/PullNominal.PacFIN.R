#' Pull the species information table and return nominal code(s)
#'
#' Pull the species information table and attempt to determine which
#' nominal codes pertain to the desired species codes.
#'
#' @inheritParams sql
#' @inheritParams getDB
#' @return
#' A unique vector of character entries, where entries are valid
#' `PACFIN_SPECIES_CODE`s. The length of the returned vector may not be the same
#' length as the supplied `pacfin_species_code` argument because duplicates are
#' removed. `NA` is returned if no values are found.
#'
PullNominal.PacFIN <- function(pacfin_species_code,
                               username = getUserName("PacFIN"),
                               password = ask_password()) {
  spp <- getDB(
    sql_species(),
    username = username,
    password = password
  )

  nom <- spp[grepl("NOM\\.", spp[, 2]), ] %>%
    # Fix a known spelling mistake
    dplyr::mutate(
      PACFIN_SPECIES_COMMON_NAME = gsub(
        "VERMILLION",
        "VERMILION",
        PACFIN_SPECIES_COMMON_NAME
      )
    )

  out <- tibble::tibble(spp) %>%
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
    dplyr::mutate(nominal = purrr::map_chr(
      searchname,
      ~ paste0(grep(.x, nom[, 2], value = TRUE), collapse = "|")
    )) %>%
    dplyr::mutate(code = purrr::map(nominal, ~ {
      if (.x[1] == "") {
        return(NA)
      }
      return(nom[grep(.x, nom[, 2]), 1])
    })) %>%
    dplyr::filter(PACFIN_SPECIES_CODE %in% pacfin_species_code) %>%
    dplyr::pull(code) %>%
    unlist() %>%
    unique()

  if (length(out) > 1) {
    out <- c(stats::na.omit(out))
  }

  return(out)
}
