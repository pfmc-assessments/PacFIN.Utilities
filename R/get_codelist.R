#' Get GRID options in PacFIN
#'
#' @return A data frame with the following six columns:
#' \itemize{
#'   \item{TYPE}{integer value of 1 (gear code), 2 (gear group), or 3 (all)}
#'   \item{GRID}{three letter grid code}
#'   \item{GROUP}{three letter gear code}
#'   \item{SHORT}{short description}
#'   \item{DESCRIPTION}{long description}
#'   \item{ENTERED}{date information was entered into table}
#' }
#'
get_codelist.GRID <- function() {
  url <- "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt"
  all <- utils::read.fwf(url(url), skip = 5, widths = c(5, 5, 6, 10, 38, 9),
    blank.lines.skip = TRUE)
  colnames(all) <- toupper(gsub("^\\s+|\\s+$| Name", "", all[1, ]))
  all <- all[c(-1, -2), ]
  all <- all[-1 * seq(grep("\\.\\.\\.", all[,3]), NROW(all)), ]
  all <- all[!grepl("^\\s+$", all[, 1]), ]
  all[ , c("TYPE", "GRID", "GROUP")] <- t(apply(all[ , c("TYPE", "GRID", "GROUP")], 1,
    function(x) gsub("^\\s*|\\s*$", "", x)))
  return(all)
}

#' Get all INPFC Areas in PacFIN
#'
#' @return A data frame with the following six columns:
#' \itemize{
#'  \item{NAME}{a short-hand version of the area description}
#'  \item{ARID}{mapping of sub area to areas}
#'  \item{TYPE}{classification of tree structure, i.e., if it is a subarea}
#'  \item{COUNCIL}{the management council that the area falls within}
#'  \item{DESCRIPTION}{detailed description of the area}
#'  \item{INPFC}{INPFC_AREA code used in PacFIN}
#' }
#'
get_codelist.INPFC <- function() {
  url <- "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt"
  all <- utils::read.fwf(url(url), skip = 4, widths = c(10, 21, 16, 8, 75))
  colnames(all) <- gsub("^\\s+|\\s+$|AREA ", "", all[1, ])
  all <- all[c(-1, -2), ]
  all <- all[grep("INPFC", all[, grep("TYPE", colnames(all))]), ]
  all[, "INPFC"] <- gsub("\\s*|\\\\|_", "", all[, grep("ARID", colnames(all))])
  return(all)
}
