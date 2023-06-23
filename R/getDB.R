#' Get a connection to a database and return a query
#'
#' Get a connection established to a database and return the information desired
#' in the specific query.
#'
#' @template sql
#' @template datasourcename
#' @param username Most often, this is a string containing your username for the
#'   database of interest. You can use the function [getUserName()], which is
#'   the default behavior, if you would prefer to not enter this argument and
#'   assume the default search and/or rules for finding your username will work.
#'   Sometimes this search will fail because of legacy rules, which are unknown
#'   to the development team, that were used to create your username. Please
#'   email the maintainer of this package if you need more functionality here.
#' @param password Most often, this is a string containing your password for
#'   the database of interest. You can use the function [ask_password()] if you
#'   would prefer to be prompted for your password. Please do not share this
#'   password with anyone or push code to a repository that has your password
#'   saved in it.
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
