#' Create column for gears according to PacFIN gears
#'
#' Data from the PacFIN
#' [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in \code{Pdata} labeled \code{geargroup}, where
#' \code{Pdata$GRID} is recoded to \code{geargroup} according to the gear table.
#'
#' @template Pdata
#' @param gears A data.frame or \code{file.path} to a \code{.csv} file
#'   that contains GRIDS according to the table taken from
#'   [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt).
#'   Path name must be the full path or in your current working directory
#'   and end in \code{.csv}. If \code{gears = NULL} then the function assumes
#'   that the appropriate table is downloaded and available in your working
#'   directory saved as \code{'GearTable.csv'}.
#' @return A modified \code{data.frame} where an additional column labeled
#'   \code{geargroups} is added to \code{Pdata}. No original columns are modified
#'   in the process.
#' @author Andi Stephens

getGearGroups = function (Pdata, gears = NULL) {

  if (is.data.frame(gears)) {

    truecols <- c("grid", "group", "agid", "gear", "agency", "PacFIN")

    sapply(seq_along(dim(gears)[2]), function(x) {

      done <- grep(truecols[x], colnames(gears)[x], ignore.case = TRUE)

      if (length(done) != 1) {

        stop(paste("The column name of the dataframe entered for gears",
          "does not match the column names for the table taken from",
          "http://pacfin.psmfs.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt",
          colnames(gears)[x], "start with (ignoring case)", truecols[x]))

      } # End if

    }) # End sapply

  } # End if is.data.frame

  if (is.character(gears)) {

    if (tail(unlist(strsplit(gears, "\\.")), 1) != "csv") {

      stop(paste("The path to the dataframe containing GRIDS information",
        "does not end in .csv, please modify accordingly."))

    } # End if tail

    if (!file.exists(gears)) {

      stop(paste("The path to the file specified in gears (", gears,
        ") does not exist. This path must be a full path,",
        "or in your working directory."))

    } else {

      gears.tmp <- read.csv(gears, as.is = TRUE)
      if (ncol(gears.tmp) == 1) gears.tmp <- read.csv(gears, header = TRUE)
      if (ncol(gears.tmp) == 1) stop(paste("gears is not reading in correctly."))
      gears <- gears.tmp

    } # End if-else

  } # End if is.character

  if (is.null(gears)) {

    if (file.exists("GearTable.csv")) {

      gears <- read.csv("GearTable.csv", as.is = TRUE)

    } else {

      stop(paste("The table GearTable.csv not present in your working",
        "directory and no argument for gears was specified,",
        "thus no geargroups were encoded."))

    } # End if-else

  } # End if is.null

  Pdata$geargroup <- gears$GROUP[match(Pdata$GRID, gears$GRID)]
  indices <- which(is.na(Pdata$geargroup))
  Pdata$geargroup[indices] <- Pdata$GRID[indices]

  return(Pdata)

} # End function getGearGroups

