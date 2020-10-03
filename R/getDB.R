#' Get a Connection to a Database and Return a Query
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
#' @author John R. Wallace, Kelli Faye Johnson
#' @return A data frame resulting from querying the database or an
#' error message because the main call is wrapped in
#' \code{\link[base]{tryCatch}}.
#'
getDB <- function(sql, datasourcename = "PacFIN",
  username = getUserName(datasourcename), password,
  asis = FALSE) {

  #### Get password and username
  if (missing(password)) {
    password <- readline(prompt = "Enter PacFIN password without quotes\n")
  }

  #### Pull from database
  database <- RODBC::odbcConnect(
    dsn = datasourcename,
    uid = username, pw = password)
  on.exit(RODBC::odbcClose(database))
  out <- tryCatch(RODBC::sqlQuery(database, sql, as.is = asis),
    error = function(e) e)

  #### Fix a column name b/c full stops aren't allowed in sql call
  #badcode
  # This might be allowed using tablename.[column.name] but KFJ
  # couldn't figure out how to do it in sql and gave up.
  if (is.data.frame(out)) {
    colnames(out) <- gsub("CATCH_LBS", "CATCH\\.LBS", colnames(out))
  }

  return(out)

}
