#' Get a list of available options, e.g., code_list, from PacFIN
#'
#' Get a data frame with options available in PacFIN and their descriptions.
#' PacFIN documentation refers to these data frames as code_lists, thus the
#' function name `get_codelist` helps explain what we are getting.
#'
#' @details S3:
#' This is an S3 class object, of which I am not very familiar with.
#' Thus, feel free to provide comments on the code or submit an
#' [issue](https://github.com/nwfsc-assess/PacFIN.Utilities/issues) if
#' you have ideas or corrections.
#'
#' @details source:
#' All PacFIN [code lists](https://pacfin.psmfc.org/pacfin_pub/codes.php)
#' are publicly available. As time progresses, I hope to have all lists
#' available within R through `code_lists`. If you have time to add
#' any of this functionality, feel free to submit a pull request and
#' thank you in advance for collaborating.
#'
#' @param x A character value specifying which code_list you want.
#' See the function call for the available lists or
#' `utils::methods("get_codelist")` for a list of all of the available
#' .S3methods.
#'
#' @export
#' @return A data frame with the following potential columns, where the
#' column names are always in all caps:
#' \itemize{
#'   \item{TYPE : }{integer value of 1 (gear code), 2 (gear group), or 3 (all)}
#'   \item{GRID : }{three letter grid or gear type code}
#'   \item{GROUP : }{three letter gear grouping code}
#'   \item{ARID : }{mapping of sub area to areas}
#'   \item{INPFC : }{International North Pacific Fisheries Commission (INPFC) Area code used in PacFIN}
#'   \item{TYPE : }{classification of tree structure, i.e., if it is a subarea}
#'   \item{COUNCIL : }{the management council that the area falls within}
#'   \item{PCID : }{character port code}
#'   \item{AGID : }{agency code, e.g., "W" for Washington}
#'   \item{PORT : }{numberic port code}
#'   \item{SHORT : }{short description}
#'   \item{DESCRIPTION : }{long, detailed description}
#' }
#' @examples
#' availablegrids <- get_codelist("GRID")
#' availableports <- get_codelist("PORT")
#'
get_codelist <- function(x = c("GRID", "INPFC", "PORT")) {
  x <- match.arg(x, several.ok = FALSE)
  UseMethod("get_codelist", object = structure(list(), class = x))
}

#' @export
#'
get_codelist.GRID <- function(x) {
  url <- "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt"
  all <- utils::read.fwf(url(url), skip = 5, widths = c(5, 5, 6, 10, 38, 9),
    blank.lines.skip = TRUE)
  colnames(all) <- toupper(gsub("^\\s+|\\s+$| Name", "", all[1, ]))
  all <- all[c(-1, -2), ]
  all <- all[-1 * seq(grep("\\.\\.\\.", all[,3]), NROW(all)), ]
  all <- all[!grepl("^\\s+$", all[, 1]), ]
  all[ , c("TYPE", "GRID", "GROUP")] <- t(apply(all[ , c("TYPE", "GRID", "GROUP")], 1,
    function(x) gsub("^\\s*|\\s*$", "", x)))
  all <- all[, -which(colnames(all) == "ENTERED")]
  return(all)
}

#' @export
#'
get_codelist.INPFC <- function(x) {
  url <- "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt"
  all <- utils::read.fwf(url(url), skip = 4, widths = c(10, 21, 16, 8, 75))
  colnames(all) <- gsub("^\\s+|\\s+$|AREA ", "", all[1, ])
  all <- all[c(-1, -2), ]
  all <- all[grep("INPFC", all[, grep("TYPE", colnames(all))]), ]
  all[, "INPFC"] <- gsub("\\s*|\\\\|_", "", all[, grep("ARID", colnames(all))])
  all[, "SHORT"] <- all[["NAME"]]
  all <- all[, c("ARID", "INPFC", "TYPE", "COUNCIL", "SHORT", "DESCRIPTION")]
  return(all)
}

#' @export
#'
get_codelist.PORT <- function(x) {
  url <- "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_ports_pcid.txt"
  all <- utils::read.fwf(
    url(url),
    skip = 9,
    widths = c(8, 9, 12, 20),
    n = 569,
    header = FALSE,
    blank.lines.skip = TRUE
  )
  colnames(all) <- c("PCID", "AGID", "PORT", "DESCRIPTION")
  all <- all[!is.na(all[["PORT"]]), ]
  return(all)
}
