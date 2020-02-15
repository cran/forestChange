\name{FCMosaic}
\alias{FCMosaic}
\title{Forest-Change Mosaic}
\description{This function tests whether adjacent layers of \code{GFC} can be
bounded together using partial matching over the layer names. If
it is possible then \code{\link{mosaic}} is implemented.}
\usage{FCMosaic(rst = NULL, lyrs = c("treecover2000", "lossyear"), mc.cores = detectCores())}
\arguments{
  \item{rst}{\code{list} or \code{NULL}. List of \code{GFC}
layers. If \code{NULL} then other arguments are
ignored and the function returns \code{NULL}.}
  \item{lyrs}{\code{character}. Vector
of strings matching layer
names in \code{GFC}
data. Defaults
\code{'treecover2000'} and
\code{'lossyear'}.}
  \item{mc.cores}{\code{logical}. Use parallel
execution. Default \code{TRUE}. Ignored
in Windows machines.}
}
\details{The function is implemented by
\code{\link{FCPolygon}} to cut \code{GFC}
data.}
\value{\code{list} of rasters.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
## Printing NULL output:    
FCMosaic(NULL)
}
