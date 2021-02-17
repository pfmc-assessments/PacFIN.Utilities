% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAge.R
\name{getAge}
\alias{getAge}
\title{Get age information for each fish}
\usage{
getAge(Pdata, verbose, keep)
}
\arguments{
\item{Pdata}{A data frame of biological samples
originating from the
Pacific Fishieries Information Network (PacFIN) data warehouse,
which originated in 2014. Data are pulled using sql calls, see
\code{\link{PullBDS.PacFIN}()}.}

\item{verbose}{A logical value specifying if output should be written
to the screen or not. Good for testing and exploring your data but
can be turned off when output indicates errors.
The printing doesn't affect any of the returned objects, instead,
printing makes it easier to see if the data has errors and what was
done to rectify them in the given function.
Sorry, but the default is to always print to the screen,
i.e., \code{verbose = TRUE}, so you do not miss out on anything.
This is standard practice for packages in the nwfsc-assess organization.}

\item{keep}{A vector of values that represent what you want to keep.
Values of \code{NA}, \code{""}, and numeric values are acceptable.
Often, it is helpful to run `unique(, useNA = "always") on the relevant
data prior to running the function to see what types are present.}
}
\value{
A vector of ages.
}
\description{
Get age information for each fish, or row of the data.
This functionality was originally in cleanAges, moved to
cleanPacFIN, and is now compartmentalized here, but called in
\link{cleanPacFIN}.
}
\details{
Age types listed in keep (or derivatives of those) are used to filter
for ages that should be kept, i.e., not returned as \code{NA}.
If \code{FISH_AGE_YEARS_FINAL} doesn't have an entry, where this should be the
best age available, then the age reads in \code{age}, \code{age2}, ... are
averaged over, if their method is acceptable, to create an entry for
\code{FISH_AGE_YEARS_FINAL}. These best ages are then returned for those
types that are included in \code{keep}.

The input argument of \code{keep} can contain a mix of numeric and character values,
but all values will be converted to character values. For example,
\itemize{
\item 1 == "B", "BB", or break and burn
\item 2 == "S" or surface read
\item 3 == scales (Oregon data)
\item 4 == "T" or thin section
\item 5 == "O" or optical scanner
\item 6 == age derived from length (Oregon data)
\item 9 == unable to read or age
\item U == unknown
}
}