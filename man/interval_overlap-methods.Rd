\name{interval_overlap}

\alias{interval_overlap}
\alias{interval_overlap,Intervals_virtual_or_numeric,Intervals_virtual_or_numeric-method}
\alias{interval_overlap,missing,ANY-method}
\alias{interval_overlap,ANY,missing-method}

\title{Assess overlap from one set of intervals to another}

\description{
  Asses overlap from intervals in one set to intervals in another set,
  and return the relevant indices.
}

\usage{
\S4method{interval_overlap}{Intervals_virtual_or_numeric,Intervals_virtual_or_numeric}(from, to, check_valid = TRUE)
}

\arguments{
  
  \item{from}{
    An \code{"Intervals"} or \code{"Intervals_full"} object, or a
    vector of class \code{"numeric"}. \emph{Note!} Prior to v. 0.11.1,
    this argument was called \code{target}.
  }
  
  \item{to}{
    An \code{"Intervals"} or \code{"Intervals_full"} object, or a
    vector of class \code{"numeric"}. \emph{Note!} Prior to v. 0.11.1,
    this argument was called \code{query}.
  }

  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? This, among other things, verifies that endpoints are
    of data type \code{"numeric"} and the \code{closed} vector/matrix is
    appropriately sized and of the correct data type. (Compiled code
    does no further checking.)
  }
  
}

\details{
  Intervals which meet at endpoints overlap only if both endpoints are
  closed. Intervals in \code{to} with \code{NA} endpoints are
  ignored, with a warning; in \code{from}, such intervals produce no
  matches. Intervals in either \code{to} or \code{from} which are
  actually empty have their endpoints set to \code{NA} before
  proceeding, with warning, and so do not generate matches. If
  eith \code{to} or \code{from} is a vector of class \code{"numeric"},
  overlap will be assess for the corresponding set of points.
}

\value{
  A list, with one element for each row/component of \code{from}. The
  elements are vectors of indices, indicating which \code{to} rows (or
  components, for the \code{"numeric"} method) overlap each interval in
  \code{from}. A list element of length 0 indicates no overlapping
  elements.
}

\note{
  If you want real (\code{type == "R"}) intervals that overlap in a set
  of positive measure --- not just at endpoints --- set all endpoints to
  open (i.e., \code{close(from) <- FALSE; closed(to) <- FALSE}) first.
  
  This function is now just a wrapper for \code{\link{which_nearest}}.
}

\seealso{
  See \code{\link{which_nearest}} for details on nearby as well as
  overlapping intervals in \code{to}.
}

\examples{
# Note that 'from' contains a valid but empty interval.

to <- Intervals(
               matrix(
                      c(
                        2,  8,
                        3,  4,
                        5, 10
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

empty(to)
empty(from)

interval_overlap(from, to)

# Non-empty real intevals of size 0 can overlap other intervals.

u <- to
type(u) <- "R"

v <- Intervals_full( rep(3,4) )
closed(v)[2,] <- FALSE
v
empty(v)
size(v)

interval_overlap(v, u)

# Working with points

interval_overlap( from, c( 2, 3, 6, NA ) )
}
