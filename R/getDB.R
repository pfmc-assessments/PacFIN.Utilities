#' Get a connection to a database and return a query
#'
#' Get a connection established to a database and return the information desired
#' in the specific query.
#'
#' @template sql
#' @template datasourcename
#' @template username
#' @template password
#' @param asis A logical, specifying whether or not to convert columns,
#' as in \code{\link[utils]{read.table}}.
#'
#' @export
#' @author John R. Wallace, Kelli F. Johnson
#' @return A data frame resulting from querying the database or an
#' error message because the main call is wrapped in
#' \code{\link[base]{tryCatch}}.
#'
getDB <- function(sql,
                  datasourcename = "PacFIN",
                  username = getUserName(datasourcename),
                  password,
                  asis = FALSE) {
  # Check that suggested package is available
  stopifnot(requireNamespace("RODBC", quietly = TRUE))

  # Get password and username
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes\n")
  }

  # Pull from database
  database <- RODBC::odbcConnect(
    dsn = datasourcename,
    uid = username,
    pw = password
  )
  on.exit(RODBC::odbcClose(database))
  out <- tryCatch(
    RODBC::sqlQuery(database, sql, as.is = asis),
    error = function(e) e
  )

  return(out)
}
