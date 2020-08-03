#' Compare length historgram of aged samples to all lengthed samples
#' 
#' @param bio.WCGBTS The dataframe from the West Coast Groundfish Bottom Trawl
#'  Survey. In the future, make this more general so that more datasets can be.
#'  used. Must include columns Year, Length_cm, and Age, but additional columns
#'  can be present.
#' @param xlim The x limits for the histogram
#' @param ylim The y limits for the histogram
#' @param max_break The maximum length
#' @param file The relative or absolute path and name of the file to write to,
#'  as a .png file.
#' @export
#' @author Ian Taylor
#' @examples {
#'   load(file.path("data", "XMPL.BDS.rda")) # may need to modify this file
#'   bds <- XMPL.BDS # rename for simplicity
#'   # rename cols so that the plot can be used
#'   bds$Year <- bds[["SAMPLE_YEAR"]]
#'   bds$Length_cm <- (bds[["FISH_LENGTH"]])*0.1 # I think these units are likely mm, but need to check.
#'   bds$Age <- bds[["FISH_AGE_YEARS_FINAL"]] # (or use age 1?)
#'   bds <- bds[!(is.na(bds$Age) &&  is.na(bds$Length_cm)), ] # if both age and length are na, don't retain.
#'   max_break <- max(bds$Length_cm, na.rm = T)
#'   state_codes <- c("W", "O", "C")
#'   names(state_codes) <- c("WA", "OR", "CA") # just for clarity
#'   for (i in state_codes) {
#'     age_representativeness_plot(bds[bds$SOURCE_AGID == i,],
#'                                 file = paste0( "agelength_bds_", i ,".png"),
#'                                 max_break = max_break,
#'                                 ylim = c(0, 0.1), # trial and error for now to figure out
#'                                 xlim = c(0, max_break))
#'   }
#' 
#' }
age_representativeness_plot <- function(bio.WCGBTS,
                                        xlim = c(0, 120),
                                        ylim = c(0, 0.049),
                                        max_break = 120,
                                        file = NULL){

  if(!is.null(file)){
    png(filename = file, width = 7, height = 7, units = 'in', res = 300)
  }
  # make multi-panel plot comparing length samples to the subset with ages
  par(mfcol = c(9, 2),
      mar = c(0.2,0.2,0.2,0.2),
      oma = c(4,4,1,1))
  # vector of years with age samples
  years <- sort(unique(bio.WCGBTS$Year))
  colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5))

  # empty plot for legend
  plot(0, type = 'n', axes = FALSE)
  legend('left',
         bty = 'n',
         fill = colvec,
         cex = 1.5,
         legend = c("All length samples",
                    "Samples with age estimates"))

  mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)

  for (y in years) {
    # make empty plot (note: xlim and ylim were set by trial and error)
    plot(0, type = 'n',
         xlim = xlim,
         xaxs = 'i',
         ylim = ylim,
         yaxs = 'i',
         axes = FALSE)
    grid()
    if (par()$mfg[2] == 1) {
      axis(2, las = 1)
    }
    if (par()$mfg[1] == par()$mfg[3] | y == max(years)) {
      axis(1)
    }
    lengths.y <- bio.WCGBTS$Length_cm[bio.WCGBTS$Year == y]
    ages.y <- bio.WCGBTS$Length_cm[bio.WCGBTS$Year == y &
                                        !is.na(bio.WCGBTS$Age)]
    lhist <- hist(lengths.y,
         breaks = seq(0, max_break, 5),
         freq = FALSE,
         col = colvec[1],
         add = TRUE)
    if (length(ages.y > 0)) {
      ahist <- hist(ages.y,
           breaks = seq(0, max_break, 5),
           freq = FALSE,
           col = colvec[2],
           add = TRUE)
    }
    p.value <- NA
    p.color <- 'grey50'
    bhat <- NA
    if (length(lengths.y) > 0 & length(ages.y) > 0) {
      p.value <- ks.test(x = lengths.y, y = ages.y)$p.value
      p.color <- ifelse(p.value > 0.05, 'green3', 'red')
      bhat <- sum(sqrt(lhist$counts/sum(lhist$counts)*ahist$counts/sum(ahist$counts)))
    }
    legend('topleft', legend = NA, bty = 'n', title = y, cex = 1.5)
    legend('right', legend = NA, bty = 'n',
           title = paste0("N lens = ",
                          length(lengths.y),
                          "\nN ages = ",
                          length(ages.y),
                          " (",
                          round(100*length(ages.y)/length(lengths.y)),
                          "%)"),
           cex = 1.0)
    legend('bottomright', legend = NA, bty = 'n',
           title = paste0("K-S p-value = ",
                          format(p.value, digits = 2)),
           cex = 1.0, title.col = p.color)
    legend('bottomright',legend = NA, bty = 'n',
           title = paste0("Bhat. coef. = ",
                          format(bhat, digits = 3),"\n"),
           cex = 1.0, title.col = "black")
  }

  if (!is.null(file)) {
    dev.off()
  }
}
