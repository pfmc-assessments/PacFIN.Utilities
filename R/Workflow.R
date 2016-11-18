##########################################################################
#' 
#' @title Workflow
#' 
#' @name Workflow
#' 
#' @author Andi Stephens, Kelli Johnson
#' 
#' @description 
#' 
#' Illustrated workflow for PacFIN biological data (BDS) workup.
#' 
#' @details
#' 
#' The diagram below is a flowchart for working up PacFIN data. 
#' 
#' The PacFIN data should be acquired from John Wallace or Andi Stephens.  If you are using 
#' CalCOM data, check with the Southwest Center Assessment team to find out who can provide
#' that data.
#' 
#' If your data are in a \code{.csv} file, read them in using:
#' \itemize{
#' \item{\code{read.csv("filename.csv", as.is=T)}}
#' }
#' If your data are in a \code{.dmp} file, read them in using:
#' \itemize{
#' \item{\code{load("filename.dmp")}}
#' }
#' 
#' 
#' @section Expansion caveats:
#' There is one manual step in the workflow. 
#' After running the expansion functions, data columns Expansion_Factor_1 and 
#' Expansion_Factor_2 are available to use in manually setting the Final_Expansion_Factor. 
#' \itemize{
#' \item{Age data are expanded separately from lengths, after running \code{cleanAges}.}
#' \item{WA fish are generally only expanded using Expansion_Factor_2.}
#' \item{Other expansions are the product of Expansion_Factor_1 * Expansion_Factor_2}
#' \item{For age-at-length comps, set Final_Expansion_Factor to 1.  Each fish represents only itself.}
#' }
#' 
#' @section Workflow: {
#' 
#' In the diagram below, datasets are represented as circles or ellipses, functions are denoted
#' by rectangles, and black and red lines represent workflow for generating age, length, or
#' age-at-length comps.  Optional paths are represented by dashed lines.
#' 
#'
#' \if{html}{\figure{Full-Workflow.png}{options: alt="Figure: Simple-Workflow.png"}}
#' \if{latex}{\figure{Full-Workflow.png}{options: width=7cm}}
#' }
#' 
#' 
NULL
