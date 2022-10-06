#' Plot cleaned fishery age- or length-composition data.
#'
#' Save several figures to your computer that summarize the composition
#' samples that are available for the given species. None of the figures
#' are meant to be included in the stock assessment document, but rather,
#' are for exploring the data.
#'
#' @template Pdata
#' @template savedir
#' @return The following figures are saved to `savedir`:
#'   * PacFIN_comp_Nbystate.png
#'   * PacFIN_comp_distributions.png
#'   * PacFIN_comp_NbyGRID.png
#'   * PacFIN_comp_depth.png
#'   * PacFIN_comp_geargroup.png
#'   * PacFIN_comp_INPFC.png
#'   * PacFIN_comp_lengthvage.png
#' @author Andi Stephens, Kelli Faye Johnson
#' 
#' @export
#'
#' @seealso This function is called by [cleanPacFIN] and heavily
#' relies on [getGearGroup] to create gear categories.
#'
plotCleaned <- function (Pdata, savedir = getwd()) {

  #### Checks
  SPID <- sort(unique(Pdata$SPID))
  if (length(SPID) > 1) {
    warning("plotCleaned is only meant to work with one species;",
      "\nfigures will be a summary of all species in your data.")
  }

  geargroups <- ifelse(length(Pdata$geargroup) > 0, TRUE, FALSE)

  #### Plot
  grDevices::png(file.path(savedir, "PacFIN_comp_Nbystate.png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2, 1),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))
  graphics::barplot(stats::xtabs(!is.na(Pdata$length) ~ Pdata$state + Pdata$fishyr),
    col = grDevices::rainbow(length(unique(Pdata$state))),
    legend.text = TRUE, xaxt = "n",
    xlab = "", ylab = "Length samples per state",
    args.legend = list(x = "topleft", bty = "n"))
  graphics::barplot(stats::xtabs(!is.na(Pdata$Age) ~ Pdata$state + Pdata$fishyr),
    col = grDevices::rainbow(length(unique(Pdata$state))),
    legend.text = FALSE,
    xlab = "Year", ylab = "Age samples per state",
    args.legend = list(x = "topleft", bty = "n"))
  
  grDevices::png(file.path(savedir, "PacFIN_comp_distributions.png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2, 1),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))
  graphics::boxplot(Pdata$lengthcm ~ Pdata$fishyr,
    xlab = "", ylab = "Length (cm)", xaxt = "n",
    frame.plot = TRUE, ylim = c(0, max(Pdata$lengthcm, na.rm = TRUE)))
  graphics::boxplot(Pdata$Age ~ Pdata$fishyr,
    xlab = "Year", ylab = "Age",
    frame.plot = TRUE, ylim = c(0, max(Pdata$Age, na.rm = TRUE)))

  grDevices::png(file.path(savedir, "PacFIN_comp_NbyGRID.png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2, 1),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))
  nGRID <- length(unique(Pdata$GRID))
  graphics::barplot(stats::xtabs(!is.na(Pdata$length) ~ Pdata$GRID + Pdata$fishyr),
    col = grDevices::rainbow(nGRID),
    legend.text = TRUE, xlab = "", xaxt = "n",
    ylab = "Length samples per gear",
    args.legend = list(x = "topleft", bty = "n", ncol = ceiling(nGRID / 4)))
  graphics::barplot(stats::xtabs(!is.na(Pdata$Age) ~ Pdata$GRID + Pdata$fishyr),
    col = grDevices::rainbow(nGRID),
    legend.text = FALSE, xlab = "Year",
    ylab = "Age samples per gear",
    args.legend = list(x = "topleft", bty = "n", ncol = ceiling(nGRID / 4)))

  if (!all(is.na(Pdata[, "DEPTH_AVG"]))) {
    grDevices::png(file.path(savedir, "PacFIN_comp_depth.png"))
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::boxplot(Pdata$DEPTH_AVG ~ Pdata$fishyr, ylab = expression(bar(Depth)),
      frame.plot = FALSE, ylim = c(0, max(Pdata$DEPTH_AVG, na.rm = TRUE)))
  }

  if (geargroups) {
    grDevices::png(file.path(savedir, "PacFIN_comp_geargroup.png"))
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mfrow = c(2, 1),
      oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
      mgp = c(1.5, 0.5, 0))
    graphics::barplot(stats::xtabs(!is.na(Pdata$length) ~ Pdata$geargroup + Pdata$fishyr),
      col = grDevices::rainbow(length(unique(Pdata$geargroup))),
      legend.text = TRUE, xaxt = "n",
      xlab = "", ylab = "Length samples per gear group",
      args.legend = list(x = "topleft", bty = "n"))
    graphics::barplot(stats::xtabs(!is.na(Pdata$Age) ~ Pdata$geargroup + Pdata$fishyr),
      col = grDevices::rainbow(length(unique(Pdata$geargroup))),
      legend.text = FALSE,
      xlab = "Year", ylab = "Age samples per gear group",
      args.legend = list(x = "topleft", bty = "n"))
  } # End if

  #### ggplots
  nn <- grDevices::rainbow(length(unique(Pdata[["state"]])))
  gg <- ggplot2::ggplot(data = Pdata,
    ggplot2::aes(x = .data[["PSMFC_ARID"]],
    fill = factor(.data[["state"]]))) +
    ggplot2::geom_bar() + ggplot2::theme_bw() +
    ggplot2::labs(fill = "State", x = "PSMFC area", y = "Count") +
    ggplot2::scale_fill_manual(values = nn)
  ggplot2::ggsave(gg,
    file = file.path(savedir, "PacFIN_comp_PSMFC.png"),
    width = 6, height = 6, dpi = 500)
  gg <- ggplot2::ggplot(data = Pdata,
    ggplot2::aes(x = .data[["lengthcm"]],
    y = .data[["Age"]])) +
    ggplot2::geom_point() + ggplot2::theme_bw() +
    ggplot2::labs(x = "Length (cm)", y = "Age (year)")
  suppressWarnings(ggplot2::ggsave(gg,
    file = file.path(savedir, "PacFIN_comp_lengthvage.png"),
    width = 6, height = 6, dpi = 500))

} # End function plotCleaned
