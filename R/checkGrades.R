#' Check Sample Numbers for Grade
#'
#' Ensure that each sample number has only one grade of fish in it.
#'
#' @inheritParams cleanPacFIN
#'
#' @author Kelli F. Johnson
#' @return todo: document what checkGrade returns.
#'
checkGrade <- function(Pdata) {
  out <- tapply(
    Pdata$GRADE, Pdata$SAMPLE_NO,
    function(x) length(unique(x)) == 1
  )
  ifelse(all(out == 1), "One grade per sample number.",
    "More than one grade per sample number."
  )
}
