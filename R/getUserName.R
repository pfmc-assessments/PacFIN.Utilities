#' Get your username for the `database` of interest
#'
#' @details
#' Use a system of rules to determine your username given the database of
#' interest. As of 2023, only support for `PacFIN` is included but future
#' versions of the codebase should support more databases such as `NORPAC`
#' to facilitate pulling information regarding the Pacific Hake or whiting
#' fishery.
#'
#' @param datasourcename Deprecated as of version 0.2.9, use `database`
#'   instead.
#' @inheritParams getDB
#' @export
#' @author Kelli F. Johnson
#' @seealso
#' * `Sys.info()`
#' @return
#' A string that can be used for `username` argument needed in `Pull*()`.
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

  un <- Sys.info()["user"]
  un_split <- strsplit(un, "\\.")[[1]]
  stopifnot(length(un_split) == 2)

  database <- match.arg(database)
  out <- switch(database,
    "PacFIN" = tolower(paste0(substr(un_split[1], 1, 1), un_split[2])),
    NA
  )

  # Some users have usernames that were created before the current rules were
  # adopted and require different usernames.
  if (un == "John.Wallace" && database == "PacFIN") {
    out <- "wallacej"
  }

  return(out)
}
