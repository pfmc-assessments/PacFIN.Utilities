#' Sample PacFIN Run
#'
#' See the example at the end of this file to get a feel for
#' the workflow for processing PacFIN data.
#' The data files for species "XMPL" are supplied with this
#' package and can be used to explore the options of
#' the various functions.
#'
#' @section Getting Started:
#' See the example catch and biological (i.e., BDS) data for
#' species XMPL. Create a \code{Pdata} data frame to work with.
#' This data frame will have all of the appropriate columns that
#' are needed for the functions within \pkg{PacFIN.Utilities}.
#'
#' @section Stratification:
#' For example, the column "geargroup" is created by
#' \code{\link{cleanPacFIN}} and will likely be used to create
#' the second-level expansions for composition data.
#' Users must create a column called \code{"stratification"}
#' that contains entries that match the column names of the
#' catch data.
#'
#' @section Expansions:
#' #1. Expand by fish ticket drop
#' #2. Expand by fishery catch
#' #3. Create sample size
#'
#' @section Compositions:
#' #1. Get the compositions \code{\link{getComps}(Pdata, Comps="LEN")}
#' #2. Do the sex ratio \code{\link{doSexRatio}()}
#' #3. Write the compositions \code{\link{writeComps}()}
#'
#' @name PacFIN_Example
#' @docType package
#'
#' @examples
#' #### Getting Started
#' data(XMPL.BDS)
#' data(Catch.XMPL)
#' Pdata <- cleanPacFIN(XMPL.BDS,
#'   keep_gears = unique(gsub("[A-Z]+\\.", "", colnames(Catch.XMPL)[-1])),
#'   keep_length_type = unique(XMPL.BDS[, "FISH_LENGTH_TYPE"]),
#'   CLEAN = FALSE)
#'
#' #### Expansions by type e.g., (LEN, AGE, AAL)
#' # Length (LEN)
#' test <- getExpansion_1(Pdata)
#' test <- getExpansion_2(test, Catch.XMPL, Units = "MT",
#'   stratification.cols = c("state", "geargroup"))
#' test$Final_Sample_Size <- capValues(test$Expansion_Factor_1_L * test$Expansion_Factor_2)
#' comps <- getComps(test[!is.na(test$lengthcm), ], Comps = "LEN")
#' comps <- doSexRatio(comps)
#' writeComps(comps)
#' unlink("out.csv")

NULL
