#' Write SQL Text for Composition Data
#'
#' Write SQL text as a single character string that will result in
#' getting the composition data from the PacFIN database.
#'
#' @template pacfin_species_code
#' @template returnsql
#' @author John R. Wallace, Kelli Faye Johnson
#'
sql.bds <- function(pacfin_species_code) {
  spid <- paste0("('", paste(pacfin_species_code, collapse = "','"), "')")
  sqlcall <- paste0(
  "Select s.*,
          samp.INPFC_AREA,
          o.UNK_NUM,
          o.UNK_WT
 ",
  # Consider changing VESSEL_NUM to VESSEL_ID b/c less NULL
          # xxx PSMFC_ARID,
          # xxx TOTAL_WGT, might be spp weight
          # xxx WGTMAX, i don't think this one matters
          # xxx WGTMIN, i don't think this one matters
          # xxx age1, done w/ reshape
          # xxx age2, done w/ reshape
          # xxx age3, done w/ reshape
          # xxx all_cluster_sum, done with ave
   "from pacfin_marts.COMPREHENSIVE_BDS_COMM s, pacfin.bds_sample_odfw o,
          pacfin.bds_sample samp
   where s.PACFIN_SPECIES_CODE = any ", spid, " 
          and s.AGENCY_CODE in ('W','O','C')
          and s.SAMPLE_NUMBER = samp.SAMPLE_NO(+)
          and s.SAMPLE_NUMBER = o.SAMPLE_NO(+)
          and s.SAMPLE_YEAR = o.SAMPLE_YEAR(+)")
  sqlcall <- gsub("\\n", "", sqlcall)
   return(sqlcall)
}
