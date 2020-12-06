#' Read PacFIN Gear Type List from URL
#' 
#' Download the list of PacFIN gears from
#' https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
#' 
#' @author Kelli Faye Johnson
#' @return A data frame of gear types with descriptions from PacFIN and 
#' the gear group for each gear type. 
#' @examples 
#' gear <- getPacFINgears()
#' rm(gear)
#' 
getPacFINgears <- function () {
  xx <- tempfile("grid", fileext = ".txt")
  utils::download.file(
    url = "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt", 
    destfile = xx, quiet = TRUE)
  out <- utils::read.fwf(xx, widths = c(5, 5, 5, 12, 35, 12), 
    skip = 7, n = 74, stringsAsFactors = FALSE)
  colnames(out) <- c("Type", "GRID", "GearGroup", "ShortName", "Description",
    "DateEntered")
  out$GRID <- gsub("\\s+", "", out$GRID)
  out$GearGroup <- gsub("\\s+", "", out$GearGroup)
  unlink(xx)
  out <- out[!(is.na(out$Type)), ]
  out <- data.frame(out, stringsAsFactors = FALSE)
  out$GearGroup <- ifelse(out$GearGroup == "ALL", 
    out$GRID, out$GearGroup)
  return(out)
}
