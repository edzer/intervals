\name{which_nearest}

\alias{which_nearest}
\alias{which_nearest,numeric,Intervals_virtual-method}
\alias{which_nearest,Intervals_virtual,Intervals_virtual-method}
\alias{which_nearest,Intervals_virtual,numeric-method}
\alias{which_nearest,numeric,numeric-method}

\title{Identify nearest member(s) in a set of intervals}

\description{
  For each point or interval in the \code{from} argument,
  identify the nearest member or members (in case of ties) of the
  interval set in the \code{to} argument.
}

\usage{
\S4method{which_nearest}{numeric,Intervals_virtual}(from, to, check_valid = TRUE)

\S4method{which_nearest}{Intervals_virtual,numeric}(from, to, check_valid = TRUE)

\S4method{which_nearest}{Intervals_virtual,Intervals_virtual}(from, to, check_valid = TRUE)

\S4method{which_nearest}{numeric,numeric}(from, to, check_valid = TRUE)
}

\arguments{
  
  \item{from}{An object of appropriate type.}

  \item{to}{An object of appropriate type.}
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }

}

\value{
  A data frame with three columns: \code{distance_to_nearest},
  \code{which_nearest}, and \code{which_overlap}. The last two are
  actually lists, since there may be zero, one, or more
  nearest/overlapping intervals in the \code{to} object for any given
  interval in \code{from}.

  Empty intervals in \code{to}, or intervals with \code{NA} endpoints,
  produce a \code{NA} distance result, and no nearest or overlapping
  hits. 
}

\note{
  (v. 0.11.0) The code used for the \code{distance_to_nearest} column
  here is completely distinct from that used for the original
  \code{\link{distance_to_nearest}} function. For the moment, they will
  co-exist for testing purposes, but this function's code will
  eventually replace the older code.

  Note that a naive way of implementing \code{which_nearest} would be to
  use the simpler, old implementation of \code{distance_to_nearest}, use
  \code{expand} to grow all intervals by the correspnoding amount, and
  then use \code{interval_overlap} to identify target. This approach,
  however, will miss a small fraction of targets due to floating point
  issues.
}

\examples{
# Point to interval. Empty rows, or those with NA endpoints, do not
# generate hits. Note that distance_to_nearest can be 0 but without
# overlap, depending on endpoint closure.

to <- Intervals_full( c(-1,0,NA,5,-1,3,10,Inf) )
closed(to)[1,] <- FALSE
closed(to)[2,2] <- FALSE
from <- c( NA, -3:5 )

to
cbind( from, which_nearest( from, to ) )

# Completely empty to object

which_nearest( from, to[1,] )

# Interval to interval

from <- Intervals( c(-Inf,-Inf,3.5,-1,1,4) )
from
which_nearest( from, to )

# Checking behavior with ties

from <- Intervals_full( c(2,2,4,4,3,3,5,5) )
closed( from )[2:3,] <- FALSE
to <- Intervals_full( c(0,0,6,6,1,1,7,8) )
closed( to )[2:3,] <- FALSE

from
to
which_nearest( from, to )

from <- Intervals_full( c(1,3,6,2,4,7) )
to <- Intervals_full( c(4,4,5,5) )
closed( to )[1,] <- FALSE

from
to
which_nearest( from, to )
}
