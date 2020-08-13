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
#' @examples
#'   data(XMPL.BDS, package = "PacFIN.Utilities")
#'   state_codes <- c("W", "O", "C")
#'   names(state_codes) <- c("WA", "OR", "CA") # just for clarity
#'   for (i in state_codes) {
#'     age_representativeness_plot(XMPL.BDS[XMPL.BDS$SOURCE_AGID == i,],
#'                                 file = paste0( "agelength_bds_", i ,".png"),
#'                                 ylim = c(0, 0.1)) # trial and error for now to figure out
#' }
age_representativeness_plot <- function(bio.WCGBTS,
                                        xlim = c(0, 155),
                                        ylim = c(0, 0.049),
                                        max_break = 155,
                                        file = NULL){
  

  #set matrix of plots subpanels (row x columns). Used later in par()
  plot_panels = c(10,2)
  
  # vector of years with age samples
  bio.WCGBTS <- changecol_pacfin(bio.WCGBTS)
  years <- sort(unique(bio.WCGBTS$Year))
  colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5))

  pc <- 1 #plot counter
  
  for (y in years) {
    
    mod <- (y-years[1]) %%  (prod(plot_panels)-1) #modulus to determine if plot if filled
    
    if(!is.null(file) && mod==0){
      if(pc != 1) dev.off()
      file_name <- strsplit(file,"[.]")[[1]][1]
      file_ext <- strsplit(file,"[.]")[[1]][2]
      png(filename = paste0(file_name,"_",pc,".",file_ext), width = 7, height = 7, units = 'in', res = 300)
      pc <- pc + 1
      
      # make multi-panel plot comparing length samples to the subset with ages
      par(mfcol = plot_panels,
          mar = c(0.2,0.2,0.2,0.2),
          oma = c(4,4,1,1))
      
      # empty plot for legend
      plot(0, type = 'n', axes = FALSE)
      legend('left',
             bty = 'n',
             fill = colvec,
             cex = 1.5,
             legend = c("All length samples",
                        "Samples with age estimates"))
      mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)
    }
    
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
    legend('bottomright',legend = NA, bty = 'n', inset=c(0,-0.25),
           title = paste0("\n","\n","Bhat. coef. = ",
                          format(bhat, digits = 3)),
           cex = 1.0, title.col = "black")
  }

  if (!is.null(file)) {
    dev.off()


#' Change Column Names of PacFIN Data To Match Survey
#'
#' Change the column names of data extracted from PacFIN to match those
#' extracted from the survey-data warehouse.
#'
#' @param data A data frame with names matching those from PacFIN.
#' @param verbose A logical value specifying if you want messages printed to
#' the screen or not. The default is \code{FALSE}, which does not print them.
#' @return A data frame with names similar to those exported by the survey-data
#' warehouse. No column are actually removed, columns are only added.
#' @export

changecol_pacfin <- function(data, verbose = FALSE) {
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
      stop("A year column was not found in your data\n",
        "Year or SAMPLE_YEAR work.")
    }
  }
  # Length column
  if (!"Length_cm" %in% colnames(data)) {
    if ("FISH_LENGTH" %in% colnames(data)) {
      data[, "Length_cm"] <- data[, "FISH_LENGTH"] * 0.1
    }
    if (!"Length_cm" %in% colnames(data)) {
      stop("A fish-length column was not found in your data\n",
        "Length_cm or FISH_LENGTH work.")
    }
  }
  # Age column
  if (!"Age" %in% colnames(data)) {
    if ("FISH_AGE_YEARS_FINAL" %in% colnames(data)) {
      data[, "Age"] <- data[, "FISH_AGE_YEARS_FINAL"]
    }
    if (!"Age" %in% colnames(data)) {
      stop("A fish-age column was not found in your data\n",
        "Age or FISH_AGE_YEARS_FINAL work.")
    }
  }
  return(data)
}
