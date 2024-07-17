#' Plot of the weight-length relationship by sex
#'
#' Plot the weight-length relationship using [ggplot2::ggplot].
#' Sex-specific relationships are plotted if available using shades of gray.
#' Note that `sex` can be missing and a single relationship for all data
#' will be plotted.
#'
#' @inheritParams getweight
#' @param weight A vector of empirical weights in kilograms.
#' @param weight.calc A vector of estimated weights output from [getweight].
#' @template lab-axes
#'
#' @author Kelli F. Johnson
#' @export
#' @seealso
#' [getWLpars] to calculate parameters of the weight-length relationship;
#' [getweight] to predict weights from empirical lengths
#'
plotWL <- function(length,
                   sex,
                   weight,
                   weight.calc,
                   xlab = "Length (cm)",
                   ylab = "Weight (kg)") {
  if (missing(sex)) {
    sex <- rep("all", length = length(length))
  }
  data <- data.frame(length, sex, weight, "pred" = weight.calc)[
    order(length),
  ]
  gg <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[["length"]], y = .data[["weight"]],
      col = .data[["sex"]]
    )
  ) +
    ggplot2::geom_point(pch = 21, alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["pred"]]),
      lwd = 1.2
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    scale_colour_grey() +
    scale_fill_grey()
  return(gg)
}
