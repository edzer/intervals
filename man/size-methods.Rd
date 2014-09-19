\name{size}

\alias{size}
\alias{size,Intervals-method}
\alias{size,Intervals_full-method}

\title{Compute interval sizes}

\description{
  Compute the size, in either Z or R as appropriate, for each interval
  in an interval matrix.
}

\usage{
\S4method{size}{Intervals}(x, as = type(x))

\S4method{size}{Intervals_full}(x, as = type(x))
}

\arguments{
  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{as}{
    Should the intervals be thought of as in Z or R? This is usually
    determined automatically from the \code{type} slot, but because
    changing type may cause object copying, it is sometimes convenient
    to temporarily override this slot without actually resetting it.
  }
  
}

\details{
  For type \code{"Z"} objects, counting measure; for type \code{"R"}
  objects, Lebesgue measure. For type \code{"Z"} objects, intervals of
  form (\emph{a},\emph{a}] and (\emph{a},\emph{a}) are both of length
  0.
}

\value{
  A numeric vector with length equal to \code{nrow(x)}.
}

\seealso{
  See \code{\link{empty}} to identify empty intervals. Note that when
  \code{type(x) == "R"}, a size of 0 does not imply that an interval is
  empty.
}

\examples{
z1 <- Intervals( cbind( 1, 1:3 ), type = "Z" )
z2 <- z1; closed(z2)[1] <- FALSE
z3 <- z1; closed(z3) <- FALSE

size(z1)
size(z2)
size(z3)

r1 <- z1; type(r1) <- "R"
r2 <- z2; type(r2) <- "R"
r3 <- z3; type(r3) <- "R"

size(r1)
size(r2)
size(r3)

s1 <- Intervals_full( matrix( 1, 3, 2 ), type = "Z" )
closed(s1)[2,2] <- FALSE
closed(s1)[3,] <- FALSE

size(s1)
}

