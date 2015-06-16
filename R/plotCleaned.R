plotCleaned = function (Pdata, length = TRUE) {
  SPID = sort(unique(Pdata$SPID))

  geargroups <- ifelse(length(Pdata$geargroup) > 0, TRUE, FALSE)

  par(mfrow = c(ifelse(geargroups, 3, 2), 2),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))

  # Number of lengths by state per year
  if (length) {
    Pdata <- Pdata[Pdata$length > -1, ]
    ylim2 <- c(0, max(Pdata$lengthcm, na.rm = TRUE))
  } else {
    ylim2 <- c(0, max(Pdata$age, na.rm = TRUE))
  }
  barplot(xtabs(Pdata$FREQ ~ Pdata$state + Pdata$fishyr), col = rainbow(3),
    legend.text = TRUE, xaxt = "n", xlab = "", ylab = "Samples per state",
    args.legend = list(x = "topleft", bty = "n"))
  mtext(side = 3, outer = TRUE, line = 1,
    paste(paste(SPID, collapse = ","), "Fishery", ifelse(length, "length", "age"),
    "compositions"))
 # Lengths

  boxplot(Pdata$lengthcm ~ Pdata$fishyr,
    ylab = ifelse(length, "Length (cm)", "Age"), xaxt = "n",
    frame.plot = FALSE, ylim = ylim2)

  # if (.Platform$OS.type == "unix") { quartz() } else { x11() }

  nGRID = length(unique(Pdata$GRID))

  barplot(xtabs(Pdata$FREQ ~ Pdata$GRID + Pdata$fishyr), col=rainbow(nGRID),
    legend.text = TRUE, xaxt = ifelse(geargroups, "n", "t"),
    ylab = "Samples per gear",
    args.legend = list(x = "topleft", bty = "n", ncol = ceiling(nGRID / 4)))

  boxplot(Pdata$DEPTH_AVG ~ Pdata$fishyr, ylab = expression(bar(Depth)),
    frame.plot = FALSE, ylim = c(0, max(Pdata$DEPTH_AVG, na.rm = TRUE)))

  if (geargroups) {

    nGG = length(unique(Pdata$geargroup))

    barplot(xtabs(Pdata$FREQ ~ Pdata$geargroup + Pdata$fishyr), col=rainbow(nGG),
      legend.text = T, xlab = "", ylab = "Samples per gear group",
      args.legend = list(x = "topleft", bty = "n"))

  } # End if

} # End function plotCleaned
