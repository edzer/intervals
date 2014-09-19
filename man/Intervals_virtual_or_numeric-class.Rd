\name{Intervals_virtual_or_numeric-class}

\docType{class}

\alias{Intervals_virtual_or_numeric-class}

\title{Class "Intervals_virtual_or_numeric"}

\description{
  A class union combining \code{"\linkS4class{Intervals_virtual}"} and
  \code{"\linkS4class{numeric}"}. Used by, e.g.,
  \code{\link{distance_to_nearest}} and \code{\link{which_nearest}}.
}

\section{Methods}{
  \describe{
    \item{distance_to_nearest}{\code{signature(from = "Intervals_virtual_or_numeric", to = "Intervals_virtual_or_numeric")}}
    \item{interval_overlap}{\code{signature(from = "Intervals_virtual_or_numeric", to = "Intervals_virtual_or_numeric")}}
  }
}


\keyword{classes}