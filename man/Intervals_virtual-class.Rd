\name{Intervals_virtual-class}

\docType{class}

\alias{Intervals_virtual-class}

\alias{closed}
\alias{closed,Intervals_virtual-method}
\alias{closed,Intervals_virtual-method}
\alias{coerce,Intervals_virtual,character-method}
\alias{head,Intervals_virtual-method}
\alias{initialize,Intervals_virtual-method}
\alias{is.na,Intervals_virtual-method}
\alias{show,Intervals_virtual-method}
\alias{t,Intervals_virtual-method}
\alias{tail,Intervals_virtual-method}
\alias{type}
\alias{type,Intervals_virtual-method}
\alias{type<-}
\alias{type<-,Intervals_virtual-method}

\title{Class "Intervals_virtual"}

\description{
  A virtual class from which the \code{"Intervals"} and
  \code{"Intervals_full"} classes derive.
}

\section{Slots}{

  \describe{

    \item{\code{.Data}:}{
      Object of class \code{"matrix"}. A two-column, numeric (see below)
      format is required. For a valid object, no value in the first
      column may exceed its partner in the second column. (Note that
      this \emph{does} permit empty interval rows, when both endpoints
      are of equal value and not both closed.) Only integral (though not
      \code{"integer"} class) endpoints are permitted if \code{type} is
      \code{"Z"}. See the note on this point in documentation for
      \code{"\linkS4class{Intervals}"}.
    }
    
    \item{\code{type}:}{
      Object of class \code{"character"}. A one-element character vector
      with either \code{"Z"} or \code{"R"} is required.
    }
    
  }
  
}

\section{Extends}{
  
  Class \code{"\linkS4class{matrix}"}, from data part.
  
  Class \code{"\linkS4class{array}"}, by class "matrix", distance 2.
  
  Class \code{"\linkS4class{structure}"}, by class "matrix", distance 3.
  
  Class \code{"\linkS4class{vector}"}, by class "matrix", distance 4,
  with explicit coerce.
  
}

\section{Methods}{
  \describe{
    \item{close\_intervals}{ \code{signature(x = "Intervals_virtual")} }
    \item{closed}{ \code{signature(x = "Intervals_virtual")} }
    \item{clusters}{ \code{signature(x = "Intervals_virtual")} }
    \item{coerce}{ \code{signature(from = "Intervals_virtual", to = "character")} }
    \item{contract}{ \code{signature(x = "Intervals_virtual")} }
    \item{expand}{ \code{signature(x = "Intervals_virtual")} }
    \item{head}{ \code{signature(x = "Intervals_virtual")} }
    \item{initialize}{ \code{signature(.Object = "Intervals_virtual")} }
    \item{interval\_complement}{ \code{signature(x = "Intervals_virtual")} }
    \item{interval\_difference}{ \code{signature(x = "Intervals_virtual", y = "Intervals_virtual")} }
    \item{interval\_intersection}{ \code{signature(x = "Intervals_virtual")} }
    \item{interval\_union}{ \code{signature(x = "Intervals_virtual")} }
    \item{is.na}{ \code{signature(x = "Intervals_virtual")} }
    \item{open\_intervals}{ \code{signature(x = "Intervals_virtual")} }
    \item{reduce}{ \code{signature(x = "Intervals_virtual")} }
    \item{show}{ \code{signature(object = "Intervals_virtual")} }
    \item{t}{ \code{signature(x = "Intervals_virtual")} }
    \item{tail}{ \code{signature(x = "Intervals_virtual")} }
    \item{type}{ \code{signature(x = "Intervals_virtual")} }
    \item{type<-}{ \code{signature(x = "Intervals_virtual")} }
    \item{which\_nearest}{ \code{signature(from = "numeric", to = "Intervals_virtual")} }
    \item{which\_nearest}{ \code{signature(from = "Intervals_virtual", to = "numeric")} }
    \item{which\_nearest}{ \code{signature(from = "Intervals_virtual", to = "Intervals_virtual")} }
  }
}

\seealso{
  See the \code{"\linkS4class{Intervals}"} and
  \code{"\linkS4class{Intervals_full}"} classes.
}

\keyword{classes}
