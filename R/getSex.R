#' Get sex codes and convert to character values
#'
#' Get current codes used for SEX and transcribe to character values only.
#' Males (1), females (2), and unknown sex (3, 9, NA, NULL) will be coded
#' as `M`, `F`, and `U`, respectively. Conversion codes were gleaned from
#' [Washington Department of Fish and Wildlife documentation](https://wdfw.wa.gov/sites/default/files/publications/01754/wdfw01754.pdf).
#'
#' @param data.vector A vector of data that contains information on SEX.
#' @template verbose
#'
#' @export
#' @seealso [cleanPacFIN] uses this function to convert the column `SEX` to
#' standardized values. Note that PacFIN does this now as well with the
#' comprehensive data base, but the function is retained for historical data
#' and to provide a summary of sexes by using `verbose = TRUE`.
#' @return A vector the same length as the input vector where all the entries
#' will be character values of either `M`, `F`, or `U`.
#'
getSex <- function(data.vector, verbose = FALSE) {

  out <- data.vector
  out[is.na(out)] <- "U"
  out[out %in% c("3", "9")] <- "U"
  out[out == "1"] <- "M"
  out[out == "2"] <- "F"

  if (verbose) {
    message("The following 'input' values to SEX were returned as 'output':")
    capture.output(
      type = "message",
      table("input" = data.vector, "return" = out, useNA = "always")
      )
  }

  return(out)

}
