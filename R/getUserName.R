#' Get User Name Based On Computer User Name
#'
#' Query the user name of your local machine to generate a user name for the
#' `database`.
#'
#' @param datasourcename Deprecated as of version 0.2.9, use `database`
#'   instead.
#' @inheritParams getDB
#' @export
#' @author Kelli F. Johnson
#' @return A single character value to use as input in functions
#' calling for \code{username = }.
#' @examples
#' \dontrun{
#' getUserName()
#' }
getUserName <- function(database = c("PacFIN"),
                        datasourcename = lifecycle::deprecated()) {

  if (lifecycle::is_present(datasourcename)) {
    lifecycle::deprecate_soft(
      when = "0.2.9",
      what = "getUserName(datasourcename)",
      with = "getUserName(database)"
    )
    database <- datasourcename
  }
  #### Get username (un) info from computer
  un <- Sys.info()["user"]
  un.split <- strsplit(un, "\\.")[[1]]
  stopifnot(length(un.split) == 2)

  #### Get desired type
  database <- match.arg(database)
  out <- switch(database,
    "PacFIN" = tolower(paste0(substr(un.split[1], 1, 1), un.split[2])),
    NA)

  #### Checks for non-common user names
  if (un == "John.Wallace" && database == "PacFIN") {
    out <- "wallacej"
  }

  return(out)
}
