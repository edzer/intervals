\name{as.matrix}

\alias{as.matrix}
\alias{as.matrix.Intervals_virtual}
\alias{as.matrix,Intervals_virtual-method}

\title{Extract matrix of endpoints}

\description{ S3 and S4 methods for extracting the matrix of endpoints
  from S4 objects. }

\usage{
\S3method{as.matrix}{Intervals_virtual}(x, ...)

\S4method{as.matrix}{Intervals_virtual}(x, ...)
}

\arguments{

  \item{x}{\code{"Intervals"} or \code{"Intervals_full"} objects.}

  \item{...}{Unused, but required by the S3 generic.}  

}

\value{
  A two-column matrix, equivalent to \code{x@.Data} or \code{as(x,
  "matrix")}.
}
