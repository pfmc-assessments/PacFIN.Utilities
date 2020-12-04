\name{XMPL.BDS}
\alias{XMPL.BDS}
\docType{data}
\title{
Example PacFIN biological dataset to use with \code{\link{PacFIN_Example}}
}
\description{
Fake data for the PacFIN \code{\link{Workflow}} demonstration.
}
\usage{data("XMPL.BDS")}
\format{
  A data frame with 20000 observations on the following 56 variables.
  \describe{
    \item{\code{SPID}}{a character vector}
    \item{\code{SAMPLE_NO}}{a character vector}
    \item{\code{SAMPLE_YEAR}}{a numeric vector}
    \item{\code{SOURCE_AGID}}{a character vector}
    \item{\code{SAMPLE_AGENCY}}{a character vector}
    \item{\code{CLUSTER_NO}}{a numeric vector}
    \item{\code{FISH_AGE_YEARS_FINAL}}{a numeric vector}
    \item{\code{FISH_AGE_CODE_FINAL}}{a numeric vector}
    \item{\code{FISH_NO}}{a numeric vector}
    \item{\code{FREQ}}{a numeric vector}
    \item{\code{FISH_LENGTH}}{a numeric vector}
    \item{\code{FISH_LENGTH_TYPE}}{a logical vector}
    \item{\code{FORK_LENGTH_ESTIMATED}}{a logical vector}
    \item{\code{FORK_LENGTH}}{a numeric vector}
    \item{\code{MATURITY}}{a character vector}
    \item{\code{MATURITY_AGCODE}}{a numeric vector}
    \item{\code{FISH_WEIGHT}}{a numeric vector}
    \item{\code{SEX}}{a character vector}
    \item{\code{DATA_TYPE}}{a character vector}
    \item{\code{DEPTH_AVG}}{a numeric vector}
    \item{\code{DEPTH_MIN}}{a numeric vector}
    \item{\code{DEPTH_MAX}}{a numeric vector}
    \item{\code{DRVID}}{a character vector}
    \item{\code{GEAR}}{a character vector}
    \item{\code{GRID}}{a character vector}
    \item{\code{MARKET_CATEGORY}}{a character vector}
    \item{\code{GRADE}}{a character vector}
    \item{\code{GRADE_AGCODE}}{a character vector}
    \item{\code{INPFC_AREA}}{a character vector}
    \item{\code{PSMFC_AREA}}{a character vector}
    \item{\code{PSMFC_ARID}}{a character vector}
    \item{\code{SAMPLE_AGID}}{a character vector}
    \item{\code{SAMPLE_MONTH}}{a numeric vector}
    \item{\code{SAMPLE_DAY}}{a numeric vector}
    \item{\code{SAMPLE_METHOD}}{a character vector}
    \item{\code{SAMPLE_TYPE}}{a character vector}
    \item{\code{MALES_WGT}}{a numeric vector}
    \item{\code{MALES_NUM}}{a numeric vector}
    \item{\code{FEMALES_NUM}}{a numeric vector}
    \item{\code{FEMALES_WGT}}{a numeric vector}
    \item{\code{TOTAL_WGT}}{a numeric vector}
    \item{\code{RWT_LBS}}{a numeric vector}
    \item{\code{LWT_LBS}}{a numeric vector}
    \item{\code{EXP_WT}}{a numeric vector}
    \item{\code{PCID}}{a character vector}
    \item{\code{PORT}}{a character vector}
    \item{\code{FTID}}{a character vector}
    \item{\code{COND}}{a character vector}
    \item{\code{COND_AGCODE}}{a character vector}
    \item{\code{WGTMAX}}{need to document}
    \item{\code{WGTMIN}}{need to document}
    \item{\code{AGE_STRUCT_AGCODE}}{a character vector}
    \item{\code{AGE_METHOD}}{a character vector}
    \item{\code{AGE_READABILITY}}{a numeric vector}
    \item{\code{AGED_BY}}{a character vector}
    \item{\code{DATE_AGED}}{a character vector}
    \item{\code{age1}}{a numeric vector}
    \item{\code{age2}}{a numeric vector}
    \item{\code{age3}}{a numeric vector}
    \item{\code{all_cluster_sum}}{a numeric vector}
    \item{\code{SPECIES_WGT}}{a numeric vector}
    \item{\code{CLUSTER_WGT}}{a numeric vector}
    \item{\code{FRAME_WGT}}{a numeric vector}
    \item{\code{ADJ_CLWT}}{a numeric vector}
  }
}
\examples{
dim(XMPL.BDS)
## maybe str(XMPL.BDS) ; plot(XMPL.BDS) ...
}
\keyword{datasets}
