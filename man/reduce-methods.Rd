\name{reduce}

\alias{reduce}
\alias{reduce,Intervals_virtual-method}

\title{Compactly re-represent the points in a set of intervals}

\description{
  In general, \code{"\linkS4class{Intervals}"} and
  \code{"\linkS4class{Intervals_full}"} objects may be redundant, the
  intervals they contain may be in arbitrary order, and they may contain
  non-informative intervals for which one or both endpoints are
  \code{NA}. The \code{reduce} function re-represents the underlying
  subsets of the integers or the real line in the unique, minimal form,
  removing intervals with \code{NA} endpoints (with warning).
}

\usage{
\S4method{reduce}{Intervals_virtual}( x, check_valid = TRUE )
}

\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }
  
}

\value{
  A single object of appropriate class, compactly representing the
  union of all intervals in \code{x}. All intervals in \code{reduce(x)}
  have numeric (i.e., not \code{NA}) endpoints.
}

\seealso{
 See \code{\link{interval_union}}, which is really just concatenates its
 arguments and then calls \code{reduce}.
}