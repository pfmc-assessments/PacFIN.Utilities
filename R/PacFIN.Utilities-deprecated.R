#' @title Deprecated function in package \pkg{pacfintools}
#' @description The functions listed below are deprecated and
#' will be removed from the package in the near future.
#' When possible, alternative functions with similar functionality are
#' available at `help("pacfintools-deprecated")`.
#' @name pacfintools-deprecated
#' @keywords internal
NULL

#' @section checkINPFC:
#' Functions related to INPFC areas have been deprecated in favor of PSMFC areas.
#' @rdname pacfintools-deprecated
#' @export
checkINPFC <- function(...) {
  lifecycle::deprecate_stop(when = "0.0.1.0005", what = "checkINPFC")
}
#' @section ls_INPFC:
#' Functions related to INPFC areas have been deprecated in favor of PSMFC areas.
#' @rdname pacfintools-deprecated
#' @export
ls_INPFC <- function(...) {
  lifecycle::deprecate_stop(when = "0.0.1.0005", what = "ls_INPFC")
}
#' @section cleanAges:
#' `cleanAges` was moved to [cleanPacFIN()]
#' @rdname pacfintools-deprecated
cleanAges <- function(...) {
  lifecycle::deprecate_stop(when = "0.2.3", what = "cleanAges")
}
