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
  "Select s.PACFIN_SPECIES_CODE,
          s.SAMPLE_YEAR,
          s.SAMPLE_AGENCY,
          s.SAMPLE_NUMBER,
          s.CLUSTER_SEQUENCE_NUMBER,
          s.FISH_ID,
          s.FISH_SEQUENCE_NUMBER,
          s.OBSERVED_FREQUENCY,
          s.AGE_SEQUENCE_NUMBER,
          s.FINAL_FISH_AGE_IN_YEARS,
          s.AGE_IN_YEARS,
          s.FINAL_FISH_AGE_CODE,
          s.FISH_LENGTH,
          s.FISH_LENGTH_TYPE_CODE,
          s.FORK_LENGTH_IS_ESTIMATED,
          s.FORK_LENGTH,
          s.FISH_MATURITY_CODE,
          s.AGENCY_FISH_MATURITY_CODE,
          s.SEX_CODE,
          s.DEPTH_AVERAGE_FATHOMS,
          s.DEPTH_MINIMUM_FATHOMS,
          s.DEPTH_MAXIMUM_FATHOMS,
          s.AGENCY_GEAR_CODE,
          s.PACFIN_GEAR_CODE,
          s.MARKET_CATEGORY,
          s.PACFIN_GRADE_CODE,
          s.AGENCY_GRADE_CODE,
          s.AGENCY_CODE,
          s.SAMPLE_MONTH,
          s.SAMPLE_DAY,
          s.SAMPLE_METHOD_CODE,
          s.SAMPLE_TYPE,
          s.WEIGHT_OF_MALES_LBS,
          s.NUMBER_OF_MALES,
          s.WEIGHT_OF_FEMALES_LBS,
          s.NUMBER_OF_FEMALES,
          s.EXPANDED_SAMPLE_WEIGHT,
          s.PACFIN_PORT_CODE,
          s.AGENCY_PORT_CODE,
          s.FTID,
          s.PACFIN_CONDITION_CODE,
          s.AGENCY_CONDITION_CODE,
          s.AGENCY_AGE_STRUCTURE_CODE,
          s.AGE_METHOD_CODE,
          s.AGE_READABILITY,
          s.PERSON_WHO_AGED,
          s.DATE_AGE_RECORDED,
          s.SPECIES_WEIGHT_LBS,
          s.ADJUSTED_CLUSTER_WEIGHT_LBS,
          s.CLUSTER_WEIGHT_LBS,
          s.FRAME_CLUSTER_WEIGHT_LBS,
          s.VESSEL_NUM,
          s.DATA_TYPE,
          s.FISH_WEIGHT,
          s.PSMFC_CATCH_AREA_CODE,
          s.IFQ_MANAGEMENT_AREA,
          s.WEIGHT_OF_LANDING_LBS,
          o.UNK_NUM,
          o.UNK_WT
 ",
  # Consider changing VESSEL_NUM to VESSEL_ID b/c less NULL
          # xxx INPFC_AREA,
          # xxx PSMFC_ARID,
          # xxx TOTAL_WGT, might be spp weight
          # xxx WGTMAX, i don't think this one matters
          # xxx WGTMIN, i don't think this one matters
          # xxx age1, done w/ reshape
          # xxx age2, done w/ reshape
          # xxx age3, done w/ reshape
          # xxx all_cluster_sum, done with ave
   "from pacfin_marts.COMPREHENSIVE_BDS_COMM s, pacfin.bds_sample_odfw o
   where s.PACFIN_SPECIES_CODE = any ", spid, " 
          and s.AGENCY_CODE in ('W','O','C')
          and s.SAMPLE_NUMBER = o.SAMPLE_NO(+)
          and s.SAMPLE_YEAR = o.SAMPLE_YEAR(+)")
  sqlcall <- gsub("\\n", "", sqlcall)
   return(sqlcall)
}
