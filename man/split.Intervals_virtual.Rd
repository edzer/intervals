\name{split}

\alias{split}
\alias{split.Intervals_virtual}
\alias{split,Intervals_virtual-method}

\title{Split an intervals object according to a factor}

\description{
  S3 and S4 methods for splitting \code{"Intervals"} or
  \code{"Intervals_full"} objects.
}

\usage{
\S3method{split}{Intervals_virtual}(x, f, drop = FALSE, ...)

\S4method{split}{Intervals_virtual}(x, f, drop = FALSE, ...)
}

\arguments{

  \item{x}{ \code{"Intervals"} or \code{"Intervals_full"} objects. }

  \item{f}{ Passed to \code{\link{split.data.frame}}. }

  \item{drop}{ Passed to \code{\link{split.data.frame}}. }
  
  \item{...}{ Passed to \code{\link{split.data.frame}}. }  

}

\value{ A list of objects of the same class as \code{x}, split by the
  levels of \code{f}. Until R 2.15, special handling was not
  required. Subsequent changes to the \pkg{base} package
  \code{\link{split}} function required an explicit method here, but
  code already provided by \code{\link{split.data.frame}} was
  sufficient. }
