% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getComps.R
\name{getComps}
\alias{getComps}
\alias{getcomps_long}
\title{Aggregate length, age, or age-at-length composition data by strata}
\usage{
getComps(
  Pdata,
  strat = NULL,
  Comps = c("AAL", "LEN", "AGE"),
  defaults = c("fleet", "fishyr", "season"),
  verbose = lifecycle::deprecated(),
  ...
)

getcomps_long(
  data,
  towstrat,
  type,
  towid = "SAMPLE_NO",
  weightid = "Final_Sample_Size_L",
  dropmissing = TRUE
)
}
\arguments{
\item{Pdata}{A data frame of biological samples
originating from the
Pacific Fishieries Information Network (PacFIN) data warehouse,
which originated in 2014. Data are pulled using sql calls, see
\code{\link{PullBDS.PacFIN}()}.}

\item{strat}{A vector of column names to stratify the output over. For
example, if you wish to summarize by ageing method, then the argument
would look like \code{strat = "agemethod"} or, if you want to look at fleets
and gear, \code{strat = c("fleet", "usegear")}.}

\item{Comps}{The type of composition data to create. See the function call
for the available options. The first option listed is the default, which
creates conditional age-at-length compositions by adding both \code{lengthcm}
and \code{Age} to the grouping structure.}

\item{defaults}{The default stratification columns which will typically be
left at their default value of \code{c("fleet", "fishyr", "season")}.}

\item{verbose}{Is deprecated as of version 0.2.8. No information is printed
to the screen from this function.}

\item{...}{Pass additional arguments to \code{getcomps_long()}, such as
\code{dropmissing = FALSE} where the default behavior is \code{dropmissing = TRUE}.
The most important argument to consider modifying is \code{getComps(weightid = "Final_Sample_Size_L")}, which will only be applicable to length data.
Instead of the default, you might want to weight each stratification by
the final sample sizes found for age data, i.e., \code{getComps(weightid = "Final_Sample_Size_A")}. You can pass \code{weightid} any column that exists in
\code{Pdata}. Legacy code required you to set \code{Final_Sample_Size} outside of a
function call with something like \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2} and all weighting was
done on the column called \code{"Final_Sample_Size"} but this is no longer the
case.}

\item{data}{A data frame. The data frame must have the column names that
are specified for the next four input arguments, \code{towstrat}, \code{type},
\code{towid}, and \code{weightid}.}

\item{towstrat}{A vector of character values providing the column names for
which you want compositions for.}

\item{type}{A character value specifying which category to summarize by,
i.e., \code{"length"} or \code{"Age"}.}

\item{towid}{A vector of character values providing the column names that
generate a unique id for each sample.}

\item{weightid}{A character value giving the column name that holds the
value to be summed for each type and strata.}

\item{dropmissing}{A logical value supplied to the \code{drop} argument in
\code{\link[stats:aggregate]{stats::aggregate()}} that specifies whether or not to keep all levels in
the data even if there are no values to report for summaries.}
}
\value{
A long data frame of weights for each grouping structure. Columns
identifying the grouping structures will come first, followed by columns
with sample sizes per group by sex category. The documentation for these
sample size columns is sparse because this function is set to be deprecated
next cycle and replaced with a simplified path to writing composition data.
But, information is present for males (sometimes abbreviated with an m),
females (sometimes abbreviated with an f), unsexed (sometimes abbreviated
with a u), and sexed (noted as both or b) records. If no sex is provided
then it is assumed all are unsexed and will be returned as such.
}
\description{
\code{getComps()} first sets up the local environment, then runs
\code{getcomps_long()}, and finally returns the results of \code{getcomps_long()},
which calculates summaries by groupings. \code{getcomps_long()} was developed to
make use of {tidyverse} but it still produces all of the old output. In the
future, this function will be replaced by code that can summarize both
survey data or commercial data. Until then, this is the best we can do and
it mimics old output such that legacy code will not break. You can and
should pass arguments to \code{getcomps_long()} using pass through arguments in
your call to \code{getComps()}, i.e., \code{...}, especially when calculating
composition data related to age. See the input arguments for more details.
}
\details{
This function uses the expansions created in \code{\link[=getExpansion_1]{getExpansion_1()}} and
\code{\link[=getExpansion_2]{getExpansion_2()}} using \code{"weightid"}. Thus, whatever column name is passed
to \code{weightid} will be used as the weight for each group. See the
documentation for the pass-through arguments, i.e., \code{...}
}
\section{Functions}{
\itemize{
\item \code{getcomps_long()}: The actual workhorse behind \code{getComps()}.

}}
\seealso{
\itemize{
\item \code{\link[=getExpansion_2]{getExpansion_2()}} should be run before this function.
\item \code{\link[=writeComps]{writeComps()}} can be run on the output from this function.
}
}
\author{
Andi Stephens, Kelli F. Johnson
}
