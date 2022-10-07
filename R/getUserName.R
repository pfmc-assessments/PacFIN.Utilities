#' Get User Name Based On Computer User Name
#'
#' Query the user name of your local machine to generate
#' a suspected user name for the \code{datasourcename}.
#'
#' @template datasourcename
#' @export
#' @author Kelli F. Johnson
#' @return A single character value to use as input in functions
#' calling for \code{username = }.
#' @examples
#' getUserName()
#'
getUserName <- function(datasourcename = c("PacFIN")) {

  #### Get username (un) info from computer
  un <- Sys.info()["user"]
  un.split <- strsplit(un, "\\.")[[1]]
  stopifnot(length(un.split) == 2)

  #### Get desired type
  datasourcename <- match.arg(datasourcename)
  out <- switch(datasourcename,
    "PacFIN" = tolower(paste0(substr(un.split[1], 1, 1), un.split[2])),
    NA)

  #### Checks for non-common user names
  if (un == "John.Wallace" && datasourcename == "PacFIN") {
    out <- "wallacej"
  }

  return(out)
}
