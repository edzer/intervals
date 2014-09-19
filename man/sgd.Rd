\name{sgd}

\alias{sgd}

\docType{data}

\title{Yeast gene model sample data}

\description{
  This data set contains a data frame describing a subset of
  the chromosome feature data represented in Fall 2007 version of
  \file{saccharomyces\_cerevisiae.gff}, available for download from the
  \emph{Saccharomyces} Genome Database (\url{http://www.yeastgenome.org}). 
}

\usage{data(sgd)}

\format{
  A data frame with 14080 observations on the following 8 variables.
  \describe{
    \item{\code{SGDID}}{SGD feature ID.}
    \item{\code{type}}{
      Only four feature types have been retatined: \code{"CDS"},
      \code{"five_prime_UTR_intron"}, \code{"intron"}, and \code{"ORF"}. Note
      that \code{"ORF"} correspond to a whole gene while \code{"CDS"}, to an
      exon. \emph{S. cerevisae} does not, however, have many
      multi-exonic genes.
    }
    \item{\code{feature_name}}{A character vector}
    \item{\code{parent_feature_name}}{
      The \code{feature_name} of the a larger element to which the
      current feature belongs. All retained \code{"CDS"} entries, for
      example, belong to an \code{"ORF"} entry.
    }
    \item{\code{chr}}{
      The chromosome on which the feature occurs.
    }
    \item{\code{start}}{Feature start base.}
    \item{\code{stop}}{Feature stop base.}
    \item{\code{strand}}{Is the feature on the Watson or Crick strand?}
  }
}

\examples{

# An example to compute "promoters", defined to be the 500 bases
# upstream from an ORF annotation, provided these bases don't intersect
# another orf. See documentation for the sgd data set for more details
# on the annotation set.

use_chr <- "chr01"

data( sgd )
sgd <- subset( sgd, chr == use_chr )

orf <- Intervals(
                 subset( sgd, type == "ORF", c( "start", "stop" ) ),
                 type = "Z"
                 )
rownames( orf ) <- subset( sgd, type == "ORF" )$feature_name

W <- subset( sgd, type == "ORF", "strand" ) == "W"

promoters_W <- Intervals(
                         cbind( orf[W,1] - 500, orf[W,1] - 1 ),
                         type = "Z"
                         )

promoters_W <- interval_intersection(
                                     promoters_W,
                                     interval_complement( orf )
                                     )

# Many Watson-strand genes have another ORF upstream at a distance of
# less than 500 bp

hist( size( promoters_W ) )

# All CDS entries are completely within their corresponding ORF entry.

cds_W <- Intervals(
                 subset( sgd, type == "CDS" & strand == "W", c( "start", "stop" ) ),
                 type = "Z"
                 )
rownames( cds_W ) <- NULL

interval_intersection( cds_W, interval_complement( orf[W,] ) )

}

\keyword{datasets}
