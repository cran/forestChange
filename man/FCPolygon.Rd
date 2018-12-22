\name{FCPolygon}
\alias{FCPolygon}
\title{Forest-Cover Polygon}
\description{This function can crop layers of Global Forest Change (\code{GFC})
using either Geographic Administrative Units (\code{GADM}) or
other user-defined polygons.}
\usage{FCPolygon(pol = NULL, lyrs = c("treecover2000", "lossyear"), 
    multicore = TRUE, ...)}
\arguments{
  \item{pol}{\code{SpatialPolygonsDataFrame}, \code{character}
or \code{NULL}. A spatial-data polygon, the name of
a \code{GADM}, or such a name plus its
corresponding higher-level unit. If \code{NULL}
then a list of \code{GADM} units is printed.}
  \item{lyrs}{\code{character}. Strings
matching layers in a
google api used to
download links of
\code{GFC}. Default
\code{'treecover2000'},
and \code{'lossyear'}.}
  \item{multicore}{\code{logical}. Use parallel
execution. Default TRUE. This is ignored in
Windows machines.}
  \item{\dots}{Additional arguments in \code{\link{getGADM}} other than
\code{'unit.nm'}. These could be \code{'level'} and/or
\code{'country'}.}
}
\details{The \code{GADM} are imported using
the in-package \code{\link{getGADM}}.
Links to the data sets are obtained using
the in-package
\code{\link{GFCurls}}. Geographic extents
in both the \code{GADM} and the \code{GFC}
are intersected using the package function
\code{\link{HansenUrltoExtent}}. Common
areas between \code{GFC} and \code{GADM}
are cropped using two functions of the
\code{\link{raster}} package:
\code{\link{crop}} and
\code{\link{rasterize}}. Depending on
localization of the \code{GADM} unit,
several \code{GFC} layers by data type
might be required. This is done
implementing the in-package
\code{\link{FCMosaic}}. This function
could be memory demanding if the extents
of the polygons used to cut the \code{GFC}
are big (30,000 km^2). For these cases,
machines with RAM of 8 GB or greater should
be used. In unix-alike systems, the
package can implement parallel execution,
see \code{\link{parallel}} package.}
\value{list of rasters or set of \code{GADM} units}
\references{Hansen, M. C., Potapov,
P. V., Moore, R., Hancher, M., Turubanova,
S. A. A., Tyukavina, A., ... & Kommareddy,
A. (2013). High-resolution global maps of
21st-century forest cover change. science,
342(6160), 850-853.}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
## A list of departments of Colombia is printed:
\donttest{
dep <- FCPolygon(level = 1)
head(dep)
}
## Two adjacent layers of GFC must be bounded together before cropping
## the GFC data using the boundaries of the the municipality of
## 'Cumaribo' in Colombia. This is automatically developed by
## FCPolygon:
\donttest{
cumariboArea <- FCPolygon(pol = 'Cumaribo')
}
## The name 'Mosquera' matchs two municipalities of Colombia. A
## corresponding department should be specified in the argument 'pol'
## of FCPolygon:
\donttest{
mosquera <- FCPolygon('Mosquera')
mosqueraNarinho <- FCPolygon(pol = c('Mosquera','Narino'))
}
}
