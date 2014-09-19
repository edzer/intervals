\name{interval_union}

\alias{interval_union}
\alias{interval_union,Intervals_virtual-method}
\alias{interval_union,missing-method}

\title{Compute the union of intervals in one or more interval matrices}

\description{
  Compute the union of intervals in one or more interval matrices. The
  intervals contained in a single interval matrix object need not, in
  general, be disjoint; \code{interval_union}, however, always returns a
  matrix with sorted, disjoint intervals.
}

\usage{
\S4method{interval_union}{Intervals_virtual}(x, ..., check_valid = TRUE)

\S4method{interval_union}{missing}(x, ..., check_valid = TRUE)
}

\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{\dots}{
    Optionally, additional objects which can be combined with
    \code{x}. See \code{\link{c.Intervals}} for details on mixing
    different types of objects.
  }
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }
  
}

\details{
  All supplied objects are combined using \code{\link[=c.Intervals]{c}}
  and then then passed to \code{\link{reduce}}. The \code{missing}
  method is only to permit use of \code{\link{do.call}} with named list,
  since no named element will typically match \code{x}.
}

\value{
  A single object of appropriate class, compactly representing the union
  of all intervals in \code{x}, and optionally, in \code{...} as
  well. For class \code{"Intervals"}, the result will have the same
  \code{closed} values as \code{x}.
}

\seealso{
 See \code{\link{reduce}}, which is used to produce the results. 
}