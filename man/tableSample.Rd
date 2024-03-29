% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableSample.R
\name{tableSample}
\alias{tableSample}
\title{Table of Sample Size By Fleet and Year}
\usage{
tableSample(
  Pdata,
  fname = paste0("fishery_", comps, "_samples.csv"),
  strat = "SOURCE_AGID",
  comps = c("LEN", "AGE"),
  remove_yrs = NULL
)
}
\arguments{
\item{Pdata}{A data frame of biological samples
originating from the
Pacific Fishieries Information Network (PacFIN) data warehouse,
which originated in 2014. Data are pulled using sql calls, see
\code{\link{PullBDS.PacFIN}()}.}

\item{fname}{A filename with the appropriate extension,
used to save the function output to the disk.
For example, \code{LenComps.csv} or
\code{file.path(getwd(), "SampleSize.csv")}.
Full, relative, or simple paths are allowed because the argument is used as
is, i.e., not redirected to a directory different than \code{getwd()}.
Note that various functions within PacFIN.Utilities have different default
values for this input argument.}

\item{strat}{A vector of column names to stratify the output over. For
example, if you wish to summarize by ageing method, then the argument
would look like \code{strat = "agemethod"} or, if you want to look at fleets
and gear, \code{strat = c("fleet", "usegear")}.}

\item{comps}{Specify whether to calculate the length or Age samples.
The default is to calculate the number of length samples.}

\item{remove_yrs}{A vector of years to remove from the data before
summarizing sample sizes. The default of \code{NULL} leads to no
sub setting of the data.}
}
\value{
A table is written to a csv file as specified in \code{fname} and the data frame
is also returned as an object invisibly.
}
\description{
Create a table of samples by fleet and year to
be included in an assessment document.
}
\author{
Chantel R. Wetzel
}
