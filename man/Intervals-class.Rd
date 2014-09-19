\name{Intervals-class}

\docType{class}

\alias{Intervals}
\alias{Intervals_full}

\alias{Intervals-class}
\alias{Intervals_full-class}

\alias{[,Intervals-method}
\alias{[,Intervals_full-method}
\alias{[<-,Intervals,ANY,missing,Intervals_virtual-method}
\alias{[<-,Intervals_full,ANY,missing,Intervals_virtual-method}
\alias{closed<-}
\alias{closed<-,Intervals-method}
\alias{closed<-,Intervals_full-method}
\alias{coerce,Intervals,Intervals_full-method}
\alias{coerce,Intervals_full,Intervals-method}
\alias{initialize,Intervals-method}
\alias{initialize,Intervals_full-method}




\title{Classes "Intervals" and "Intervals_full"}

\description{
  \code{"Intervals"} objects are two-column matrices which represent
  sets, possibly non-disjoint and in no particular order, of intervals
  on either the integers or the real line. All intervals in each object
  have the same endpoint closure pattern. \code{"Intervals_full"}
  objects are similar, but permit interval-by-interval endpoint closure
  specification.
}

\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("Intervals",
    \dots)}, or better, by using the constructor functions
    \code{\link{Intervals}(\dots)} and
    \code{\link{Intervals_full}(\dots)}.
}

\section{Slots}{
  
  \describe{

    \item{\code{.Data}:}{See \code{"\linkS4class{Intervals_virtual}"}.}
    
    \item{\code{closed}:}{
      For \code{"Intervals"} objects, a two-element logical vector. For
      \code{"Intervals_full"} objects, a two-column logical matrix with
      the same dimensions as \code{.Data}. If omitted in a \code{new}
      call, the \code{closed} slot will be initialized to an object of
      appropriate type and size, with all entries \code{TRUE}. If
      \code{closed} is a vector of length 1, or a vector of length 2 for
      the \code{"Intervals_full"} class, an appropriate object will be
      made by reusing the supplied values row-wise. See the example
      below.
    }

    \item{\code{type}:}{See \code{"\linkS4class{Intervals_virtual}"}.}
        
  }
  
}

\section{Extends}{
  
  Class \code{"\linkS4class{Intervals_virtual}"}, directly.

  Class \code{"\linkS4class{matrix}"}, by class
  \code{"Intervals_virtual"}, distance 2.

  Class \code{"\linkS4class{array}"}, by class
  \code{"Intervals_virtual"}, distance 3.

  Class \code{"\linkS4class{structure}"}, by class
  \code{"Intervals_virtual"}, distance 4.

  Class \code{"\linkS4class{vector}"}, by class
  \code{"Intervals_virtual"}, distance 5, with explicit coerce.
  
}

\seealso{
  See \code{"\linkS4class{Intervals_virtual}"}.
}

\section{S3 methods}{
  
  As of R 2.8.1, it still does not seem possible to write S4 methods for
  \code{rbind} or \code{c}. To concatenate sets of intervals into a
  single sets, the S3 methods \code{\link{c.Intervals}} and
  \code{\link{c.Intervals_full}} are provided. While \code{rbind} might
  seem more natural, its S3 dispatch is non-standard and it could not be
  used. Both methods are documented separately.
  
}

\section{S4 methods}{
  
  \describe{
    
    \item{[}{\code{ signature(x = "Intervals")} }
    \item{[}{\code{ signature(x = "Intervals_full")} }
    \item{[<-}{\code{ signature(x = "Intervals", i = "ANY", j = "missing", value = "Intervals_virtual")} }
    \item{[<-}{\code{ signature(x = "Intervals_full", i = "ANY", j = "missing", value = "Intervals_virtual")} }
    \item{adjust\_closure}{\code{ signature(x = "Intervals")} }
    \item{adjust\_closure}{\code{ signature(x = "Intervals_full")} }
    \item{closed<-}{\code{ signature(x = "Intervals")} }
    \item{closed<-}{\code{ signature(x = "Intervals_full")} }
    \item{coerce}{\code{ signature(from = "Intervals", to = "Intervals_full")} }
    \item{coerce}{\code{ signature(from = "Intervals_full", to = "Intervals")} }
    \item{empty}{\code{ signature(x = "Intervals")} }
    \item{empty}{\code{ signature(x = "Intervals_full")} }
    \item{initialize}{\code{ signature(.Object = "Intervals")} }
    \item{initialize}{\code{ signature(.Object = "Intervals_full")} }
    \item{size}{\code{ signature(x = "Intervals")} }
    \item{size}{\code{ signature(x = "Intervals_full")} }

    % \item{\code{[}:}{
    %   When used to subset rows, class is preserved; when used to subset
    %   columns, the \code{closed} and \code{type} slots are discarded and
    %   an appropriately subset version of \code{.Data} is returned. See
    %   example below.
    % }
    % 
    % \item{\code{closed<-}:}{
    %   Replacement accessor for \code{closed} slot. See the example
    %   below. See description of the \code{closed} slot above for details
    %   on how one- or two-element logical vectors are interpreted.
    % }
    % 
    % \item{\code{coerce}:}{
    %   Coercion methods are provided for converting back and forth
    %   between \code{"Intervals"} and \code{"Intervals_full"}
    %   objects. See example below. An error will be generated when
    %   attemption to down-class a \code{"Intervals_full"} object which
    %   does not have the same closure settings for every interval.
    %   
    %   A coercion method is also provided for pretty character strings.
    % }
    
  }
  
}

\note{
  We do not currently permit an integer data type for the endpoints
  matrix, even when \code{type == "Z"}, because this creates
  complications when taking complements -- which is most easily handled
  through the use of \code{-Inf} and \code{Inf}. This is particularly
  awkward for objects of class \code{"Intervals"}, since current endpoint
  closure settings may not permit inclusion of the minimal/maximal
  integer. This issue may be addressed, however, in future updates. (We
  do, however, check that endpoints are congruent to 0 mod 1 when
  \code{type == "Z"}.)

  When creating object, non-matrix endpoint sources will be converted to
  a two-column matrix, for convenience. Recycling is supported for the
  \code{closed} slot when creating new objects.
}

\section{Warning}{
  Validity checking takes place when, for example, using the
  \code{type<-} replacement accessor: if one attempts to set type to
  \code{"Z"} but the endpoint matrix contains non-integer values, an
  error is generated. Because accessors are not used for the endpoint
  matrix itself, though, it is possible to create invalid \code{"Z"}
  objects by setting endpoints to inappropriate values.
}

\examples{

# The "Intervals" class

i <- Intervals(
               matrix(
                      c(1,2,  
                        3,5,
                        4,6,
                        8,9
                        ),
                      byrow = TRUE,
                      ncol = 2
               ),
               closed = c( TRUE, TRUE ),
               type = "Z"
               )

# Row subsetting preserves class. Column subsetting causes coercion to
# "matrix" class. 

i
i[1:2,]
i[,1:2]

# Full endpoint control

j <- as( i, "Intervals_full" )
closed(j)[ 3:4, 2 ] <- FALSE
closed(j)[ 4, 1 ] <- FALSE
j

# Rownames may be used

rownames(j) <- c( "apple", "banana", "cherry", "date" )
j

# Assignment preserves class, coercing if necessary

j[2:3] <- i[1:2,]
j

}

\keyword{classes}
