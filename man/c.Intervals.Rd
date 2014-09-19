\name{c}

\alias{c}
\alias{c.Intervals}
\alias{c.Intervals_full}

\title{Combine different interval matrix objects}

\description{
  S3 methods for concatenating sets of intervals into a single set.
}

\usage{
\S3method{c}{Intervals}(...)
\S3method{c}{Intervals_full}(...)
}

\arguments{
  
  \item{...}{\code{"Intervals"} or \code{"Intervals_full"} objects.}
  
}

\details{
  
  All objects are expected to have the same value in the \code{type}
  slot. If the \code{closed} slots differ for
  \code{"\linkS4class{Intervals}"} objects and \code{type == "Z"}, the
  objects will be adjusted to have \code{closed} values matching that of
  \code{x}; if \code{type == "R"}, however, then all objects must first
  be coerced to class \code{"\linkS4class{Intervals_full}"}, with a
  warning. This coercion also occurs when a mixture of object types is
  passed in. A \code{NULL} in any argument is ignored.
  
}

\value{
  A single \code{"\linkS4class{Intervals}"} or
  \code{"\linkS4class{Intervals_full}"} object. Input objects are
  concatenated in their order of appearance in the the argument list.
  
  If any input argument is not a set of intervals, \code{list(...)} is
  returned instead.
}

\note{
  These methods will be converted to S4 once the necessary dispatch on
  \code{...} is supported.
}

\examples{
f1 <- Intervals( 1:2, type = "Z" )
g1 <- open_intervals( f1 + 5 )

# Combining Intervals objects over Z may require closure adjustment
c( f1, g1 )

f2 <- f1; g2 <- g1
type( f2 ) <- type( g2 ) <- "R"

# Combine Intervals objects over R which have different closure requires
# coercion

h <- c( f2, g2 )

# Coercion for mixed combinations as well
c( h, g2 + 10 )

\dontrun{
# Combining different types is not permitted
c( h, g1 + 10 )
}

}