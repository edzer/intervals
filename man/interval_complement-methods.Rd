\name{interval_complement}

\alias{interval_complement}
\alias{interval_complement,Intervals_virtual-method}

\title{Compute the complement of a set of intervals}

\description{
  Compute the complement of a set of intervals.
}

\usage{
\S4method{interval_complement}{Intervals_virtual}(x, check_valid = TRUE)
}

\arguments{  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }
}

\value{
  An object of the same class as \code{x}, compactly representing the
  complement of the intervals described in \code{x}.
}

\note{
  For objects of class \code{"Intervals"}, closure on \code{-Inf} or
  \code{Inf} endpoints is set to match that of all the intervals with
  finite endpoints. For objects of class \code{"Intervals_full"},
  non-finite endpoints are left open (although in general, this package
  does not make a distinction between closed and open infinite
  endpoints). 
}

