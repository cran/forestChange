\name{FCMask}
\alias{FCMask}
\title{Forest-Change Mask}
\description{This function can format \code{GFC} into forest-distribution
regions of interest over time.}
\usage{FCMask(pol = NULL, year = 1:2, cummask = TRUE, deforest = FALSE, 
    perc = 80:100, mc.cores = detectCores(), ...)}
\arguments{
  \item{pol}{\code{RasterStack};
\code{SpatialPolygonsDataFrame}; \code{Extent};
\code{character}, or \code{NULL}. Spatial object
such as any of these produced by \code{getGADM}
or by \code{FCPolygon}. If \code{NULL} then a
list of unit names is printed, see
\code{getGADM}.}
  \item{year}{\code{numeric}. Years between 0 and 18 (or between
2000 and 2018).}
  \item{cummask}{\code{logical}. Compute cumulative masks
instead of discrete masks. Default \code{TRUE}.}
  \item{deforest}{\code{logical}. Process non-forest areas
instead of forest areas. Default \code{FALSE}.}
  \item{perc}{\code{numeric}.  Percentage of canopy
closure. Default \code{80:100}. This argument is
ignored if \code{deforest = TRUE}.}
  \item{mc.cores}{\code{numeric}. The number of cores,
see \code{\link{mclapply}}.}
  \item{\dots}{Additional arguments in \code{\link{FCPolygon}}.}
}

\value{\code{RasterBrick} of forest/non-forest masks over time. or
\code{character} vector with \code{GADM}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
## A mask of GFC layers in the municipality of 'Uribia' is computed:
 \donttest{
 UribiaMask <- FCMask('Uribia')
 plot(UribiaMask)
 }
}
