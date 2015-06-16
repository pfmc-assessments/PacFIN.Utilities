plotCleaned = function (Pdata) {

  SPID = sort(unique(Pdata$SPID))

  geargroups <- ifelse(length(Pdata$geargroup) > 0, TRUE, FALSE)

  par(mfrow = c(ifelse(geargroups, 3, 2), 2),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))
  # Number of lengths by state per year

  lenData = Pdata[Pdata$length > -1,]
  barplot(xtabs(lenData$FREQ ~ Pdata$state + Pdata$fishyr), col=rainbow(3),
          legend.text = T, xlab="Year", ylab = "Length Samples",
          main=paste("Species", SPID))

 # Lengths

  boxplot(lenData$length ~ Pdata$fishyr, ylab="Lengths",
          main=paste("Species", SPID))

  # if (.Platform$OS.type == "unix") { quartz() } else { x11() }

  nGRID = length(unique(Pdata$GRID))

  barplot(xtabs(Pdata$FREQ ~ Pdata$GRID + Pdata$fishyr), col=rainbow(nGRID),
          legend.text = T, xlab = "Year", ylab = "Gears",
          main=paste("Species", SPID))

  if ( length(Pdata$geargroup) > 0 ) {

    nGG = length(unique(Pdata$geargroup))

    barplot(xtabs(Pdata$FREQ ~ Pdata$geargroup + Pdata$fishyr), col=rainbow(nGG),
            legend.text = T, xlab = "Year", ylab = "Gear Groups",
            main=paste("Species", SPID))

  } # End if

  boxplot(Pdata$DEPTH_AVG ~ Pdata$fishyr, ylab="AVG_DEPTH",
          main=paste("SPECIES", SPID))

} # End function plotCleaned
