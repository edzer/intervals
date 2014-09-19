\name{interval_difference}

\alias{interval_difference}
\alias{interval_difference,Intervals_virtual,Intervals_virtual-method}

\title{Compute set difference}

\description{
  Compute the set difference between two objects.
}

\usage{
\S4method{interval_difference}{Intervals_virtual,Intervals_virtual}(x, y, check_valid = TRUE)
}

\arguments{  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{y}{
    An \code{"Intervals"} or \code{"Intervals_full"} object, with a
    \code{type} slot matching that of \code{x}.
  }
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called on \code{x} and \code{y}
    before passing to compiled code? Also see
    \code{\link{interval_overlap}}.
  }
}

\value{
  An object representing the subset of the integers or real line, as
  determined by \code{type(x)}, found in \code{x} but not in \code{y}.
}

\seealso{
  These methods are just wrappers for
  \code{\link{interval_intersection}} and
  \code{\link{interval_complement}}.
}