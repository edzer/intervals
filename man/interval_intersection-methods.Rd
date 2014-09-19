\name{interval_intersection}

\alias{interval_intersection}
\alias{interval_intersection,Intervals_virtual-method}
\alias{interval_intersection,missing-method}

\title{Compute the intersection of one or more sets of intervals}

\description{
  Given one or more sets of intervals, produce a new set compactly
  representing points contained in at least one interval of each input
  object.
}

\usage{
\S4method{interval_intersection}{Intervals_virtual}(x, ..., check_valid = TRUE)

\S4method{interval_intersection}{missing}(x, ..., check_valid = TRUE)
}

\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{\dots}{Additional objects of the same classes permitted for \code{x}.}
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }

}

\value{
  A single object representing points contained in each of the objects
  supplied in the \code{x} and \code{...} arguments.
}

\seealso{
  See \code{\link{interval_union}} and
  \code{\link{interval_complement}}, which are used to produce the
  results. 
}