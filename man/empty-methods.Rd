\name{empty}

\alias{empty}
\alias{empty,Intervals-method}
\alias{empty,Intervals_full-method}

\title{Identify empty interval rows}

\description{
  A valid interval matrix may contain empty intervals: those with common
  endpoints, at least one of which is open. The \code{empty} method
  identifies these rows.
}

\usage{
\S4method{empty}{Intervals}(x)

\S4method{empty}{Intervals_full}(x)
}

\arguments{
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
}

\details{
  Intervals are deemed to be empty when their endpoints are equal and
  not both closed, or for \code{type == "Z"}, when their endpoints differ
  by 1 and both are open. The matrices \code{x} and \code{x[!empty(x),]}
  represent the same subset of the integers or the real line.
}

\value{
  A boolean vector with length equal to \code{nrow(x)}.
}

\section{Warning}{
  Exact equality (\code{==}) comparisons are used by \code{empty}. See
  the package vignette for a discussion of equality and floating point
  numbers.
}

\note{
  Note that intervals of size 0 may not be empty over the reals, and
  intervals whose second endpoint is strictly greater than the first
  \emph{may} be empty over the integers, if both endpoints are open.
}

\seealso{
  See \code{\link{size}} to compute the size of each interval in an
  object.
}

\examples{
z1 <- Intervals( cbind( 1, 1:3 ), type = "Z" )
z2 <- z1; closed(z2)[1] <- FALSE
z3 <- z1; closed(z3) <- FALSE

empty(z1)
empty(z2)
empty(z3)

r1 <- z1; type(r1) <- "R"
r2 <- z2; type(r2) <- "R"
r3 <- z3; type(r3) <- "R"

empty(r1)
empty(r2)
empty(r3)

s1 <- Intervals_full( matrix( 1, 3, 2 ), type = "Z" )
closed(s1)[2,2] <- FALSE
closed(s1)[3,] <- FALSE

empty(s1)
}

