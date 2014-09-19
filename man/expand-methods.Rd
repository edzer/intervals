\name{expand}

\alias{expand}
\alias{expand,Intervals_virtual-method}

\alias{contract}
\alias{contract,Intervals_virtual-method}

\title{Expand or contract intervals}

\description{
  It is often useful to shrink or grow each interval in a set of
  intervals: to smooth over small, uninteresting gaps, or to address
  possible imprecision resulting from floating point arithmetic. The
  \code{expand} and \code{contract} methods implement this, using either
  absolute or relative difference.
}

\usage{
\S4method{expand}{Intervals_virtual}(x, delta = 0, type = c("absolute", "relative"))

\S4method{contract}{Intervals_virtual}(x, delta = 0, type = c("absolute", "relative"))
}


\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{delta}{
    A non-negative adjustement value. A vector is permitted,
    and its entries will be recycled if necessary.
  }
  
  \item{type}{
    Should adjustment be based on relative or absolute difference.  When
    \code{type == "relative"} intervals are expanded/contracted to
    include/exclude points for which a relative difference with respect
    to the nominal value is less than or equal to \code{delta}. (See the
    note below.) When \code{type == "absolute"}, absolute rather than
    relative difference is used, i.e., all intervals are expanded or
    contracted by the same amount.
  }

}

\value{
  A single object of appropriate class, with endpoint positions adjusted
  as requested. Expansion returns an object with the same dimension as
  \code{x}; contraction may lead to the elimination of now-empty rows.
}

\note{
  Here, the relative difference between \emph{x} and \emph{y} is
  |\emph{x} - \emph{y}|/max(|\emph{x}|, |\emph{y}|).
}

\examples{
# Using adjustment to remove small gaps

x <- Intervals( c(1,10,100,8,50,200), type = "Z" )
close_intervals( contract( reduce( expand(x, 1) ), 1 ) )

# Finding points for which, as a result of possible floating point
# error, intersection may be ambiguous. Whether y1 intersects y2[2,]
# depends on precision.

delta <- .Machine$double.eps^0.5
y1 <- Intervals( c( .5, 1 - delta / 2 ) )
y2 <- Intervals( c( .25, 1, .75, 2 ) )

# Nominal 

interval_intersection( y1, y2 )

# Inner limit

inner <- interval_intersection(
                               contract( y1, delta, "relative" ),
                               contract( y2, delta, "relative" )
                               )

# Outer limit

outer <- interval_intersection(
                               expand( y1, delta, "relative" ),
                               expand( y2, delta, "relative" )
                               )

# The ambiguous set, corresponding to points which may or may not be in
# the intersection -- depending on numerical values for endpoints
# which are, with respect to relative difference, indistinguishable from
# the nominal values.

interval_difference( outer, inner )
}