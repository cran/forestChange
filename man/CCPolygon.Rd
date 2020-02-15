\name{CCPolygon}
\alias{CCPolygon}
\title{Canopy-cover polygon}
\description{This function computes regions of interest across \code{GFC} areas
for tree-cover data (\code{GFC30TC}).}
\usage{CCPolygon(pol = NULL, path, int.patt = "[[:digit:]].tif", mc.cores = detectCores(), 
    ...)}
\arguments{
  \item{pol}{\code{SpatialPolygonsDataFrame}, \code{character}
or \code{NULL}. Polygon geometry, \code{GADM}, stack
such as tha produced by \code{FCPolygon}, or
\code{NULL}. If \code{NULL} then a list of
\code{GADM} units is printed.}
  \item{path}{\code{character}. File path(s) to \code{GFC30TC} layers
(.tif), or to zip archives containing \code{GFC30TC}.}
  \item{int.patt}{\code{character}. If files in
\code{path} are compressed then
common pattern in the extracted
files.  Default
\code{'[[:digit:]].tif'}.}
  \item{mc.cores}{\code{logical}. Use parallel
execution. Default \code{TRUE}. Ignored
in Windows machines.}
  \item{\dots}{Additional arguments in \code{\link{FCMask}}.}
}

\value{\code{RasterStack}, set of \code{GADM} units, or \code{NULL}.}
\references{Sexton, J. O., Song, X. P.,
Feng, M., Noojipady, P., Anand, A., Huang,
C., ... & Townshend, J. R. (2013). Global,
30-m resolution continuous fields of tree
cover: Landsat-based rescaling of MODIS
vegetation continuous fields with lidar-based
estimates of error. International Journal of
Digital Earth, 6(5), 427-448.}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
mun  <- CCPolygon(NULL)
}
