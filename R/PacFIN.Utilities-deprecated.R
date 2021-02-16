#' @title Deprecated function in package \pkg{PacFIN.Utilities}
#' @description The functions listed below are deprecated and
#' will be removed from the package in the near future.
#' When possible, alternative functions with similar functionality are
#' available at `help("PacFIN.Utilities-deprecated")`.
#' @name PacFIN.Utilities-deprecated
#' @keywords internal
NULL

#' @section checkINPFC:
#' Functions related to INPFC areas have been deprecated in favor of PSMFC areas.
#' @rdname PacFIN.Utilities-deprecated
#' @export
checkINPFC <- function(...) {
  lifecycle::deprecate_stop(when = "0.0.1.0005", what = "checkINPFC")
}
#' @section ls_INPFC:
#' Functions related to INPFC areas have been deprecated in favor of PSMFC areas.
#' @rdname PacFIN.Utilities-deprecated
#' @export
ls_INPFC <- function(...) {
  lifecycle::deprecate_stop(when = "0.0.1.0005", what = "ls_INPFC")
}