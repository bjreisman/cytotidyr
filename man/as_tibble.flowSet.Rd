% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asdataframe_flowSet.R
\name{as_tibble.flowSet}
\alias{as_tibble.flowSet}
\title{S3 Method for the as_tibble function to convert a flowset into a tibble via a data.frame}
\usage{
\method{as_tibble}{flowSet}(x, ...)
}
\arguments{
\item{x}{a flowSet or ncdfFlowSet object}

\item{add_filename}{if TRUE adds the filename of the original FCS file as a column}

\item{use_longnames}{if TRUE, uses the longName from the FCS file as the column names}

\item{add_pdata}{if TRUE, adds pData as additional columns to the output}

\item{verbose}{if TRUE, will display warnings, otherwise will hide warnings}
}
\value{
A tibble of all the cells in the flowSet
}
\description{
S3 Method for the as_tibble function to convert a flowset into a tibble via a data.frame
}
