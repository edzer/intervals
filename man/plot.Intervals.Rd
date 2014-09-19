\name{plot.Intervals}

\alias{plot}
\alias{plot.Intervals}
\alias{plot.Intervals_full}
\alias{plot,Intervals,missing-method}
\alias{plot,Intervals_full,missing-method}
\alias{plot,Intervals,ANY-method}
\alias{plot,Intervals_full,ANY-method}

\title{Plotting methods for interval objects}

\description{
  S3 methods for plotting \code{"Intervals"} and \code{"Intervals_full"}
  objects.
}

\usage{
\S3method{plot}{Intervals}(x, y, ...)
\S3method{plot}{Intervals_full}(
     x, y = NULL,
     axes = TRUE,
     xlab = "", ylab = "",
     xlim = NULL, ylim = NULL,
     col = "black", lwd = 1,
     cex = 1,
     use_points = TRUE,
     use_names = TRUE,
     names_cex = 1,
     ...
     )

\S4method{plot}{Intervals,missing}(x, y, ...)
\S4method{plot}{Intervals_full,missing}(x, y, ...)
\S4method{plot}{Intervals,ANY}(x, y, ...)
\S4method{plot}{Intervals_full,ANY}(x, y, ...)
}

\arguments{
  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
  

  \item{y}{
    Optional vector of heights at which to plot intervals. If omitted,
    \code{y} will be automatically computed to generate a compact plot
    but with no overlap.
  }


  \item{axes}{As for \code{\link{plot.default}}.} 
  
  \item{xlab}{As for \code{\link{plot.default}}.}

  \item{ylab}{As for \code{\link{plot.default}}.}
 
  \item{xlim}{As for \code{\link{plot.default}}.}
  
  \item{ylim}{
    If not explicitly supplied, \code{ylim} is set to the maximum value
    required for intervals which are visible for the given \code{xlim}.
  }
  
  \item{col}{
    Color used for segments and endpoint points and interiors. Recycled
    if necessary.
  }

  \item{lwd}{Line width for segments. See \code{\link{par}}.}

  \item{cex}{
    Endpoint magnification. Only relevant if \code{use_points =
      TRUE}. See \code{\link{par}}.
  }
  
  \item{use_points}{Should points be plotted at interval endpoints?}
  
  \item{use_names}{
    Should \code{rownames(x)} by used for segment labels in the plot?
  }
  
  \item{names_cex}{
    Segment label magnification. Only relevant if \code{use_names =
      TRUE}.
  }

  \item{...}{Other arguments for \code{\link{plot.default}}.}
  
}

\details{
  Intervals with \code{NA} for either endpoint are not
  plotted. Vertical placement is on the integers, beginning with 0.
}

\value{None.}

\examples{
# Note plot symbol for empty interval in 'from'.

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
                  closed = c( FALSE, TRUE ),
                  type = "Z"
                  )

rownames(from) <- c("a","b","c","d","e")

to <- Intervals(
                matrix(
                       c(
                         2,  8,
                         3,  4,
                         5, 10
                         ),
                       ncol = 2, byrow = TRUE
                       ),
                closed = c( FALSE, TRUE  ),
                type = "Z"
                )

rownames(to) <- c("x","y","z")

empty(from)

plot(
     c(from,to),
     col = rep(1:2, c(nrow(from), nrow(to)))
     )

legend("topright", c("from","to"), col=1:2, lwd=1)

# More intervals. The maximal height shown is adapted to the plotting
# window.

B <- 10000
left <- runif( B, 0, 1e5 )
right <- left + rexp( B, rate = 1/10 )
x <- Intervals( cbind( left, right ) )

plot(x, use_points=FALSE)
plot(x, use_points=FALSE, xlim = c(0, 500))
}