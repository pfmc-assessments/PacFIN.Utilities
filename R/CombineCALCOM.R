#' Combine CalCOM and PacFIN data
#'
#' Adds required PacFIN columns to CalCOM data, initializing them
#' to meaningful values as appropriate, translating from the CalCOM values when
#' they exist in a different format.
#' 
#' @param Pdata a PacFIN dataset
#' @param CalCOM a CalCOM dataset
#' 
#' @return Returns a combined dataset in PacFIN format.
#' 
#' @export
#'
#' @author Melissa Haltuch, Andi Stephens

combineCalCOM = function ( Pdata, CalCOM ) {

  .Defunct("combineCalCOM", package = "PacFIN.Utilities",
    msg = "CalCOM data is available in PacFIN.")

} # End CombineCalCOM
