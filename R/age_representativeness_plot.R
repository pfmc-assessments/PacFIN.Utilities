#' Compare length histogram of aged samples to all lengthed samples
#'
#' @param bio The dataframe containing the lengths and ages. Currently the
#' function works with data from the West Coast Groundfish Bottom Trawl
#' Survey extracted by the nwfscSurvey package as well as PacFIN data.
#' Must include either the columns Year, Length_cm, and Age, or,
#' for PacFIN data, SAMPLE_YEAR, FISH_LENGTH, and FISH_AGE_YEARS_FINAL.
#' Additional columns can be present.
#' @param xlim The x limits for the histogram
#' @param ylim The y limits for the histogram
#' @param max_break The maximum length in cm (If PacFIN data is provided,
#' FISH_LENGTH is automatically converted from mm to cm).
#' @param file The relative or absolute path and name of the file to write to,
#'  as a .png file.
#' @param plot_panels A vector of two integers that determine the matrix
#' dimensions of the plots panels (row x columns), i.e., used in \code{par()}
#' if \code{!is.null(file)}.
#' @param wait2plot A logical specifying if you want R to ask you to confirm
#' page changes between plots. The default is \code{FALSE} and it will also
#' be \code{FALSE} if plots are saved to the disk.
#' 
#' @details
#' Output on figures include K-S p-value, which is the p-value output from 
#' \code{\link{ks.test}} and is colored red if <.05 and green if >=.05, and 
#' \code{expression(hat(b))}, which is the bhattacharyya coefficient calculated 
#' as \code{sum(sqrt(pi1*pi2))} where pi1 and pi2 are the proportion of data in
#' bin i for each of the two distributions. 
#'
#' @return
#' Several plots, potentially, are printed to the screen or saved to the disk
#'
#' @export
#' @author Ian G. Taylor, Kathryn L. Doering, Brian Langseth, Kelli F. Johnson
age_representativeness_plot <- function(bio,
                                        xlim = c(0, max_break),
                                        ylim = c(0, 0.049),
                                        max_break = 155,
                                        file = NULL,
                                        plot_panels = c(10, 2),
                                        wait2plot = FALSE) {
  if (!is.null(file)) {
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  if (!is.null(file)) wait2plot <- FALSE

  bio <- changecol_pacfin(bio, verbose = FALSE)
  bio <- bio[!(is.na(bio$Age) && is.na(bio$Length_cm)), ]
  if (nrow(bio) == 0) {
    stop("Your data does not have any length or age data.")
  }
  if (any(bio[, "Length_cm"] > max_break, na.rm = TRUE)) {
    nrows_old <- nrow(bio)
    bio <- bio[bio[, "Length_cm"] < max_break, ]
    warning(
      nrows_old - nrow(bio),
      " out of ",
      nrows_old,
      " length values are larger than max_break and will be excluded."
    )
  }
  years <- sort(unique(bio$Year))

  colvec <- c(grDevices::rgb(1, 0, 0, alpha = 0.8), grDevices::rgb(0, 0, 1, alpha = 0.5))

  pc <- 1 # plot counter
  for (y in years) {
    mod <- (y - years[1]) %% (prod(plot_panels)) # modulus to determine if plot if filled
    if (!is.null(file) && mod == 0) {
      if (pc != 1) grDevices::dev.off()
      file_name <- strsplit(file, "[.]")[[1]][1]
      file_ext <- strsplit(file, "[.]")[[1]][2]
      grDevices::png(filename = paste0(file_name, "_", pc, ".", file_ext), width = 7, height = 7, units = "in", res = 300)
      pc <- pc + 1

      # make multi-panel plot comparing length samples to the subset with ages
      graphics::par(
        mfcol = plot_panels,
        mar = c(0.2, 0.2, 0.2, 0.2),
        oma = c(4, 4, 1, 1)
      )
    }
    if (is.null(file)) graphics::par(oma = c(1, 1, 1, 1), ask = wait2plot)

    # make empty plot (note: xlim and ylim were set by trial and error)
    plot(0,
      type = "n",
      xlim = xlim,
      xaxs = "i", xlab = "",
      ylim = ylim,
      yaxs = "i", ylab = "",
      axes = FALSE
    )
    graphics::grid()
    if (graphics::par()$mfg[2] == 1) {
      graphics::axis(2, las = 1)
    }
    if (graphics::par()$mfg[1] == graphics::par()$mfg[3] | y == max(years)) {
      graphics::axis(1)
    }
    lengths.y <- bio$Length_cm[bio$Year == y]
    ages.y <- bio$Length_cm[bio$Year == y &
      !is.na(bio$Age)]
    lhist <- graphics::hist(lengths.y,
      breaks = seq(0, max_break, 5),
      freq = FALSE,
      col = colvec[1],
      add = TRUE
    )
    if (length(ages.y > 0)) {
      ahist <- graphics::hist(ages.y,
        breaks = seq(0, max_break, 5),
        freq = FALSE,
        col = colvec[2],
        add = TRUE
      )
    }
    p.value <- NA
    p.color <- "grey50"
    bhat <- NA
    if (length(lengths.y) > 0 & length(ages.y) > 0) {
      p.value <- suppressWarnings(
        classes = "warning",
        stats::ks.test(x = lengths.y, y = ages.y)$p.value
      )
      p.color <- ifelse(p.value > 0.05, "green3", "red")
      bhat <- sum(sqrt(lhist$counts / sum(lhist$counts) * ahist$counts / sum(ahist$counts)))
    }
    graphics::legend("topleft", legend = NA, bty = "n", title = y, cex = 1.5)
    myinset <- ifelse(is.null(file), 0, -0.25)
    myinset2 <- ifelse(is.null(file), -0.02, -0.35)
    graphics::legend("bottomright",
      legend = "\n\n", bty = "n", inset = c(0, myinset),
      title = paste0(
        length(lengths.y), " lengths\n",
        length(ages.y), " ages (",
        round(100 * length(ages.y) / length(lengths.y)),
        "%)"
      ),
      cex = 1.0
    )
    graphics::legend("bottomright",
      legend = "\n", bty = "n", inset = c(0, myinset),
      title = paste0(
        "K-S p-value = ",
        sprintf("%.3f", p.value)
      ),
      cex = 1.0, title.col = p.color
    )
    graphics::legend("bottomright",
      legend = NA, bty = "n", inset = c(0, myinset2),
      title = as.expression(bquote(atop(phantom(), hat(b) == .(sprintf("%.3f", bhat))))),
      cex = 1.0, title.col = "black"
    )
    if ((graphics::par()$mfg[1] == plot_panels[1] & graphics::par()$mfg[2] == plot_panels[2]) |
      is.null(file) | y == max(years)) {
      # empty plot for legend
      par.old <- graphics::par(no.readonly = TRUE)
      graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      graphics::legend("bottomright",
        xpd = TRUE, inset = c(0, -0.01), horiz = FALSE,
        bty = "n",
        fill = colvec,
        cex = 1.0,
        legend = c("All lengths", "Lengths with ages")
      )
      graphics::mtext("Length (cm)", side = 1, line = -1.0, outer = FALSE)
      graphics::par(par.old)
    }
  }
}


#' Change Column Names of PacFIN Data To Match Survey
#'
#' Change the column names of data extracted from PacFIN to match those
#' extracted from the survey-data warehouse.
#'
#' @param data A data frame with names matching those from PacFIN.
#' @template verbose
#' @return A data frame with names similar to those exported by the survey-data
#' warehouse. No column are actually removed, columns are only added.
#' @export

changecol_pacfin <- function(data, verbose = TRUE) {
  if (!"SAMPLE_YEAR" %in% colnames(data)) {
    if (verbose) {
      message("Returning original data because SAMPLE_YEAR wasn't a column.")
    }
    return(data)
  }
  # Year column
  if (!"Year" %in% colnames(data)) {
    if ("SAMPLE_YEAR" %in% colnames(data)) {
      data[, "Year"] <- data[, "SAMPLE_YEAR"]
    }
    # Code other options for year column here
    if (!"Year" %in% colnames(data)) {
      stop(
        "A year column was not found in your data\n",
        "Year or SAMPLE_YEAR work."
      )
    }
  }
  # Length column
  if (!"Length_cm" %in% colnames(data)) {
    if ("FISH_LENGTH" %in% colnames(data)) {
      # convert PacFIN FISH_LENGTH from mm to cm
      data[, "Length_cm"] <- data[, "FISH_LENGTH"] * 0.1
    }
    if (!"Length_cm" %in% colnames(data)) {
      stop(
        "A fish-length column was not found in your data\n",
        "Length_cm or FISH_LENGTH work."
      )
    }
  }
  # Age column
  if (!"Age" %in% colnames(data)) {
    if ("FISH_AGE_YEARS_FINAL" %in% colnames(data)) {
      data[, "Age"] <- data[, "FISH_AGE_YEARS_FINAL"]
    }
    if (any("AGE" %in% colnames(data))) {
      data <- dplyr::rename("Age" = "AGE")
    }
    if (!"Age" %in% colnames(data)) {
      stop(
        "A fish-age column was not found in your data\n",
        "Age or FISH_AGE_YEARS_FINAL work."
      )
    }
  }
  return(data)
}
