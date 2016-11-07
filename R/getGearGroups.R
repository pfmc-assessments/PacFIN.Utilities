###########################################################################
#
#' Create column for gears according to PacFIN gears
#'
#' Data from the PacFIN
#' [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in \code{Pdata} labeled \code{geargroup}, where
#' \code{Pdata$GRID} is recoded to \code{geargroup} according to the gear table.
#'
#' @template Pdata
#' 
#' @details
#' 
#'   GRIDS according to the table taken from
#'   [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' 
#' @return A modified \code{data.frame} where an additional column labeled
#'   \code{geargroups} is added to \code{Pdata}. No original columns are modified
#'   in the process.
#' @author Andi Stephens
#' 
############################################################################

getGearGroups = function (Pdata) {

cat("\n\nGear groupings reflect those in the table at",
       "http://pacfin.psmfs.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt\n\n")

  Pdata$geargroup <- GearTable$GROUP[match(Pdata$GRID, GearTable$GRID)]
  indices <- which(is.na(Pdata$geargroups))
  Pdata$geargroup[indices] <- Pdata$GRID[indices]

  return(Pdata)

} # End function getGearGroups

