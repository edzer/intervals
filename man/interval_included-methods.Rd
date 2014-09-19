\name{interval_included}

\alias{interval_included}
\alias{interval_included,Intervals,Intervals-method}
\alias{interval_included,Intervals_full,Intervals_full-method}

\title{Assess inclusion of one set of intervals with respect to another}

\description{
  Determine which intervals in the one set are completely included in
  the intervals of a second set.
}

\usage{
\S4method{interval_included}{Intervals,Intervals}(from, to, check_valid = TRUE)
\S4method{interval_included}{Intervals_full,Intervals_full}(from, to, check_valid = TRUE)
}

\arguments{
  
  \item{from}{
    An \code{"Intervals"} or \code{"Intervals_full"} object, or a
    vector of class \code{"numeric"}.
  }
  
  \item{to}{
    An \code{"Intervals"} or \code{"Intervals_full"} object, or a
    vector of class \code{"numeric"}.
  }

  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? This, among other things, verifies that endpoints are
    of data type \code{"numeric"} and the \code{closed} vector/matrix is
    appropriately sized and of the correct data type. (Compiled code
    does no further checking.)
  }
  
}

\value{
  A list, with one element for each row/component of \code{from}. The
  elements are vectors of indices, indicating which \code{to} rows (or
  components, for the \code{"numeric"} method) are completely included
  within each interval in \code{from}. A list element of length 0
  indicates no included elements. Note that empty \code{to} elements are
  not included in anything, and empty \code{from} elements do not
  include anything.
}

\seealso{
  See \code{\link{interval_overlap}} for partial overlaps -- i.e., at at
  least a point.
}

\examples{
# Note that 'from' and 'to' contain valid but empty intervals.

to <- Intervals(
               matrix(
                      c(
                        2,  6,
                        2,  8,
                        2,  9,
                        4,  4,
                        6,  8
                        ),
                      ncol = 2, byrow = TRUE
                      ),
               closed = c( TRUE, FALSE ),
               type = "Z"
               )

from <- Intervals(
               matrix(
                      c(
                         2,  8,
                         8,  9,
                         6,  9,
                        11, 12,
                         3,  3
                        ),
                      ncol = 2, byrow = TRUE
                      ),
               closed = c( TRUE, FALSE ),
               type = "Z"
               )
rownames(from) <- letters[1:nrow(from)]

from
to
interval_included(from, to)

closed(to) <- TRUE
to
interval_included(from, to)

# Intervals_full

F <- FALSE
T <- TRUE

to <- Intervals_full(
                     rep( c(2,8), c(4,4) ),
                     closed = matrix( c(F,F,T,T,F,T,F,T), ncol = 2 ),
                     type = "R"
                     )

type( from ) <- "R"
from <- as( from, "Intervals_full" )

from
to
interval_included(from, to)

# Testing

B <- 1000

x1 <- rexp( B, 1/1000 )
s1 <- runif( B, max=5 )
x2 <- rexp( B, 1/1000 )
s2 <- runif( B, max=3 )

from <- Intervals_full( cbind( x1, x1 + s1 ) )
to <- Intervals_full( cbind( x2, x2 + s2 ) )

ii <- interval_included( from, to )
ii_match <- which( sapply( ii, length ) > 0 )

from[ ii_match[1:3], ]
lapply( ii[ ii_match[1:3] ], function(x) to[x,] )

included <- to[ unlist( ii ), ]
dim( included )

interval_intersection( included, interval_complement( from ) )
}
