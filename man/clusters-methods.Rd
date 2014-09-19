\name{clusters}

\alias{clusters}
\alias{clusters,numeric-method}
\alias{clusters,Intervals_virtual-method}

\title{Identify clusters in a collection of positions or intervals}

\description{
  This function uses tools in the \pkg{intervals} package to quickly
  identify clusters -- contiguous collections of positions or intervals
  which are separated by no more than a given distance from their
  neighbors to either side.
}

\usage{
\S4method{clusters}{numeric}(x, w, which = FALSE, check_valid = TRUE)

\S4method{clusters}{Intervals_virtual}(x, w, which = FALSE, check_valid = TRUE)
}

\arguments{
  
  \item{x}{An appropriate object.}

  \item{w}{
    Maximum permitted distance between a cluster member and its
    neighbors to either side.
  }

  \item{which}{
    Should indices into the \code{x} object be returned instead of
    actual subsets?
  }

  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}} and
    \code{\link{reduce}}.
  }
  
}

\details{
  A cluster is defined to be a maximal collection, with at least two
  members, of components of \code{x} which are separated by no more than
  \code{w}. Note that when \code{x} represents intervals, an interval
  must actually \emph{contain a point} at distance \code{w} or less from
  a neighboring interval to be assigned to the same cluster. If the ends
  of both intervals in question are open and exactly at distance
  \code{w}, they will not be deemed to be cluster co-members. See the
  example below.
}

\note{
  Implementation is by a call to \code{\link{reduce}} followed by a call
  to \code{\link{interval_overlap}}. The \code{clusters} methods are
  included to illustrate the utility of the core functions in the
  \pkg{intervals} package, although they are also useful in their own
  right.
}

\value{
  A list whose components are the clusters. Each component is thus a
  subset of \code{x}, or, if \code{which == TRUE}, a vector of
  indices into the \code{x} object. (The indices correspond to row
  numbers when \code{x} is of class \code{"Intervals_virtual"}.)
}

\examples{
# Numeric method
w <- 20
x <- sample( 1000, 100 )
c1 <- clusters( x, w )

# Check results
sapply( c1, function( x ) all( diff(x) <= w ) )
d1 <- diff( sort(x) )
all.equal(
          as.numeric( d1[ d1 <= w ] ),
          unlist( sapply( c1, diff ) )
          )

# Intervals method, starting with a reduced object so we know that all
# intervals are disjoint and sorted.
B <- 100
left <- runif( B, 0, 1e4 )
right <- left + rexp( B, rate = 1/10 )
y <- reduce( Intervals( cbind( left, right ) ) )

gaps <- function(x) x[-1,1] - x[-nrow(x),2]
hist( gaps(y), breaks = 30 )

w <- 200
c2 <- clusters( y, w )
head( c2 )
sapply( c2, function(x) all( gaps(x) <= w ) )

# Clusters and open end points. See "Details".
z <- Intervals(
               matrix( 1:4, 2, 2, byrow = TRUE ),
               closed = c( TRUE, FALSE )
               )
z
clusters( z, 1 )
closed(z)[1] <- FALSE
z
clusters( z, 1 )
}