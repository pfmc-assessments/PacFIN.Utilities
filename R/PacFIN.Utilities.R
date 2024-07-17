#' PacFIN.Utilities: Functions for working up PacFIN data
#'
#' The PacFIN.Utilities package provides functions for filtering, summarizing, expanding, and compiling
#' composition data, and writing out the final products:
#' * length-
#' * age- and
#' * age-at-length compositions.
#'
#' Many of the functions described below write summary output to the console, and several create plots for
#' visualizing data at various steps in processing.  The information written to the console
#' can be captured using the [sink] function to encapsulate your workflow:
#'
#' `sink("myfilename.txt", split=TRUE)`
#'
#' ... do work ...
#'
#' `sink()`
#'
#' The second call closes the file.
#'
#' @section Filtering functions:
#' [cleanPacFIN] Filters raw PacFIN data
#'
#' @section Summary functions:
#' [plotRawData] Plots commonly viewed raw PacFIN data
#'
#' [plotCleaned] Plots the product of \code{cleanPacFIN}
#'
#' In addition, the [getExpansion_1] function has a "plot" argument to create
#' plots documenting the expansions it creates.  It also writes summary output to the
#' console.
#'
#' @section Optional functions:
#' [getSeason] For treating data from seasonal fisheries, such as Petrale.
#'
#' @section Expansion functions:
#' [getExpansion_1] creates the values for the sample to tow expansion.
#'
#' [getExpansion_2] computes the expansion values from the tow upwards to a
#' user-specified stratification
#'
#' @section Expansion caveats:
#' There is one manual step in the workflow.
#' After running the expansion functions, data columns Expansion_Factor_1 and
#' Expansion_Factor_2 are available to use in manually setting the Final_Expansion_Factor.
#' * Age data are expanded separately from lengths
#' * WA fish are generally only expanded using Expansion_Factor_2.
#' * Other expansions are the product of Expansion_Factor_1 * Expansion_Factor_2
#' * For age-at-length comps, set Final_Expansion_Factor to 1.  Each fish represents only itself.
#'
#' @section Composition functions:
#' [getComps] uses the column specified in `weightid` and the user-specified
#' stratification to create comps.
#'
#' [doSexRatio] separates the unsexed fish into males and females
#'
#' [writeComps] formats the composition data for SS3 and writes it to a file.
#'
#' @section Infrequently used functions:
#' You can run these manually to reset the initial values.
#'
#' [getState] Run by [cleanPacFIN] to initialized the `state` column.
#'
#' [getGearGroup] Run by [cleanPacFIN] to initialize `geargroups`.
#'
#' [capValues] Used to limit the maximum value in a vector.
#'
#' @docType package
#' @name PacFIN.Utilities
NULL
