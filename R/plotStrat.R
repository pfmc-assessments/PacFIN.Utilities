#' Plot Length Distributions with Aged and All Fish
#' 
#' todo: document
#' 
#' @param data A data frame
#' @param dir The directory you want to print the plots
#' @param npages The number of pages you want the plots split over.
#' @param col.fleet Fleet column name.
#' @param col.area Area column name.
#' @param col.age Age column name.
#' @param col.length Length column name.
#' @param height Figure height.
#' @param width Figure width.
plotStrat <- function(
  data,
  dir = getwd(),
  npages = 5,
  col.fleet = "fleet",
  col.area = "state",
  col.age = "Age",
  col.length = "lengthcm",
  height = 10, width = 10
  ) {

  splits <- split(unique(data$year)[order(unique(data$year))], 
    ggplot2::cut_number(unique(data$year)[order(unique(data$year))], npages))
  data[["fleet"]] <- factor(data[[col.fleet]])
  data[["area"]] <- factor(data[[col.area]])
  data[["Age"]] <- as.factor(!is.na(data[[col.age]]))
  for (ii_g in c(stats::formula("area ~ fleet"), stats::formula("fleet ~ area"))) {
    grDevices::pdf(file = file.path(dir, paste0("lengthedages_", 
      gsub("~", "", paste(ii_g, collapse = "")), ".pdf")),
      height = height, width = width)
    for(ii in seq_along(splits)) {
      plotmea <- data[data$year %in% splits[[ii]], , drop = FALSE]
      if (nrow(plotmea) == 0) next
    gg <- ggplot2::ggplot(plotmea,
      ggplot2::aes(
        x = .data[[col.length]],
        y = year,
        group = interaction(year,Age),
        fill = Age
        )
      ) +
      ggridges::geom_density_ridges2(scale = 5, alpha = 0.7) +
      ggplot2::facet_grid(ii_g) +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = guide_legend(title = "Aged")) +
      ggplot2::theme(
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "top"
        )
      print(gg)
    }

    grDevices::dev.off()
  }
  return(invisible(gg))
}
