#' Write SQL text for composition data
#'
#' Write SQL text as a single character string that will result in
#' getting the composition data from the PacFIN database.
#'
#' @template pacfin_species_code
#' @template returnsql
#' @author Kelli F. Johnson
#'
sql.bds <- function(pacfin_species_code) {
  # Consider changing 
  # * VESSEL_NUM to VESSEL_ID b/c less NULL
  # * TOTAL_WGT, might be spp weight
  # * WGTMAX, i don't think this one matters
  # * WGTMIN, i don't think this one matters
  # * all_cluster_sum, done with ave
  spid <- paste0("('", paste(pacfin_species_code, collapse = "','"), "')")
  sqlcall <- glue::glue("
   SELECT *
   FROM PACFIN_MARTS.COMPREHENSIVE_BDS_COMM
   WHERE PACFIN_SPECIES_CODE = any {spid}
   "
  )
  sqlcall <- gsub("\\n", " ", sqlcall)
  return(sqlcall)
}
