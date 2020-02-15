\name{EBVMask}
\alias{EBVMask}
\title{EBV Mask}
\description{This function combines EBV distribution maps with
forest/non-forest masks from \code{\link{FCMask}}.}
\usage{EBVMask(pol = NULL, path, int.patt = "[[:digit:]].tif", mc.cores = detectCores(), 
    ...)}
\arguments{
  \item{pol}{\code{SpatialPolygonsDataFrame}, \code{character}
or \code{NULL}. User-defined polygon, the name of a
\code{GADM}, or such a name plus its corresponding
higher-level unit. If \code{NULL} then a list of
\code{GADM} units is printed.}
  \item{path}{\code{character}. File path(s) to the distribution maps
maps.  (.tif). Compressed files (.zip) are extracted.}
  \item{int.patt}{\code{character}. If files in
\code{path} are compressed then
common pattern in the extracted
files.  Default
\code{'[[:digit:]].tif'}.}
  \item{mc.cores}{\code{numeric}. The number of cores.}
  \item{\dots}{additional arguments in \code{\link{FCMask}}. This
argument only works if \code{pol} is not an object produced
by \code{\link{FCMask}}.}
}

\value{\code{RasterStack}, set of \code{GADM} units, or \code{NULL}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
mun  <- EBVMask(NULL)
}
