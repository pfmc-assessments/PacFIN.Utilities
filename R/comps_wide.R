#' Convert long data frame with composition data to wide data frame
#'
#' Convert a long data frame with one column for proportions, one
#' column for the bin each proportion belongs to, and stratification
#' columns that inform the model what time, sex, fleet, etc. that the
#' data come from.
#'
#' @template data
#' @template breaks
#' @param col_bins The column name that pertains the bin or group you
#' wish to transform from long to wide, i.e., every unique value in this
#' column will be a new column in the wide format.
#' @param col_proportions The column name that pertains to the proportion
#' data in the data frame. This column will be used as information for each
#' new column in the wide format, i.e., summed across created categories.
#' @template includeplusgroup
#'
#' @return A data frame with initial columns for each stratification present
#' in the data, i.e., all columns not included in
#' `col_bins` and `col_proportions`. Followed by one column for each bin
#' present in the data that sums the proportional information for each
#' combination of stratification and bin. This data frame can often be used
#' as is in Stock Synthesis, though the stratification columns may need to
#' be altered such that sample sizes are correct or added if missing from
#' the stratification levels.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' temp <- data.frame(
#'   state = rep(c("WA", "OR"), length.out = 30),
#'   year = rep(2010:2015, each = 5),
#'   Age = rep(1:15, 2),
#'   ap = rlnorm(n = 30))
#' comps <- comps_wide(temp, breaks = 3:8, col_proportions = "ap")
#' testthat::expect_equal(NCOL(comps), 8)
#' \dontrun{
#' print(comps)
#' }
#'
comps_wide <- function(data, breaks,
  col_bins = "Age",
  col_proportions = "lf",
  includeplusgroup = TRUE) {

  col_proportions.num <- which(colnames(data) == col_proportions)
  col_bins.num <- which(colnames(data) == col_bins)
  stopifnot(length(col_proportions.num) == 1)
  stopifnot(length(col_bins.num) == 1)
  data <- data[, c(seq_along(data)[-c(col_bins.num, col_proportions.num)],
    col_bins.num, col_proportions.num)]
  data[, NCOL(data) - 1] <- comps_bins(
    vector = data[, NCOL(data) - 1, drop = TRUE],
    breaks = breaks, includeplusgroup = includeplusgroup,
    returnclass = "numeric"
    )

  outformula <- stats::formula(paste(col_proportions, "~ ."))
  out <- stats::reshape(direction = "wide", stats::aggregate(outformula, data = data, sum),
    idvar = colnames(data)[1:(NCOL(data)-2)],
    timevar = colnames(data)[NCOL(data)-1])
  out[is.na(out)] <- 0
  out <- out[do.call(order,
    as.list(out[, colnames(out)[-(NCOL(out):(NCOL(out)-1))]])), ]

  return(out)
}
