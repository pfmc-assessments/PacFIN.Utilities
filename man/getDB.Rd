% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDB.R
\name{getDB}
\alias{getDB}
\title{Get a connection to a database and return a query}
\usage{
getDB(
  sql,
  database = "PacFIN",
  datasourcename = lifecycle::deprecated(),
  username,
  password,
  asis = FALSE
)
}
\arguments{
\item{sql}{A valid SQL statement.}

\item{database}{A string providing the name of the database that you wish to
connect to. This string must match the name that is used to configure the
database on your computer exactly. The default is \code{"PacFIN"} which will
lead to pulling data from the Pacific Fisheries Information Network.}

\item{datasourcename}{Deprecated as of version 0.2.9. Please use the
\code{database} argument instead.}

\item{username}{Most often, this is a string containing your username for the
database of interest. You can use \code{\link[=getUserName]{getUserName()}} if you prefer to not
enter this argument and assume the default search and/or rules for finding
your username will work. This is the default behavior if you leave
\code{username} as a missing argument, i.e., \code{username <- getUserName(database = database)}. Sometimes this search will fail because of legacy rules, which
are unknown to the development team, that were used to create your
username. Please email the maintainer of this package if you need more
functionality here.}

\item{password}{Most often, this is a string containing your password for
the database of interest. You can use the function \code{\link[=ask_password]{ask_password()}} if you
would prefer to be prompted for your password. Please do not share this
password with anyone or push code to a repository that has your password
saved in it.}

\item{asis}{A logical, specifying whether or not to leave columns as is
rather than convert them to factors, as defined in \code{\link[RODBC:sqlQuery]{RODBC::sqlGetResults()}}
and \code{\link[utils:read.table]{utils::read.table()}}. This logical is first passed to
\code{\link[RODBC:sqlQuery]{RODBC::sqlQuery()}}, then to \code{\link[RODBC:sqlQuery]{RODBC::sqlGetResults()}}, and finally to
\code{\link[utils:read.table]{utils::read.table()}}, where inside of \code{\link[utils:read.table]{utils::read.table()}}, if the
logical is a single value, it is repeated for every column present, and
thus, you do not need to know how many columns the resulting data set is.
The default behavior within this package is \code{FALSE}, which does \emph{NOT} leave
the columns as is. In fact, if you set \code{asis = TRUE} many functions within
this package will fail because it leads to all columns in the downloaded
data frame being of the character class causing downstream calculations
that assume numerical output to fail because they are characters.}
}
\value{
A data frame resulting from querying the database or an
error message because the main call is wrapped in
\code{\link[base]{tryCatch}}.
}
\description{
Get a connection established to a database and return the information desired
in the specific query.
}
\author{
John R. Wallace, Kelli F. Johnson
}
