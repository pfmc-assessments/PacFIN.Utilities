#' Write SQL Text for Area
#'
#' Write SQL text as a single character string that will result in
#' getting the area data from the PacFIN database.
#'
#' @template returnsql
#' @author John R. Wallace, Kelli F. Johnson
#'
sql.area <- function() {
  sqlcall <- "Select * from PACFIN.BDS_ar"
  return(sqlcall)
}
