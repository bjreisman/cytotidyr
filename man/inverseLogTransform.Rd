% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inverseLogTransform.R
\name{inverseLogTransform}
\alias{inverseLogTransform}
\title{Create the definition of the inverse of a log transformation function (base specified by
user) to be applied on a data set}
\usage{
logTransform(transformationId="defaultLogTransform", logbase=10, r=1, d=1)
}
\arguments{
\item{transformationId}{character string to identify the transformation}

\item{logbase}{positive double that corresponds to the base of the
logarithm.}

\item{r}{positive double that corresponds to a scale factor.}

\item{d}{positive double that corresponds to a scale factor}
}
\value{
Returns an object of class \code{transform}.
}
\description{
Create the definition of the inverse of a log transformation function (base specified by
user) to be applied on a data set
}
\seealso{
\code{\link{transform-class}}, \code{\link{transform}}

Other Transform functions: 
\code{\link{inverseArcsinhTransform}()}
}
\concept{Transform functions}
\keyword{methods}
