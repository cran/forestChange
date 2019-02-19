\name{EBVmetric}
\alias{EBVmetric}
\title{EBV metric}
\description{This function can compute metrics of two Essential Biodiversity
Variables (\code{EBV} metrics): forest extents and forest
fragmentation indices using Global Forest Change data
(\code{GFC}).}
\usage{EBVmetric(tc, met = "forest.ext", year = 0, perc = 80)}
\arguments{
  \item{tc}{\code{RasterStack}. \code{GFC} layers such as that
produced by \code{\link{FCPolygon}}.}
  \item{met}{\code{character}. Name of a \code{EBV}
metric, see \code{Details} section. Partial
matching is supported. Default
\code{'forest.ext'}.}
  \item{year}{\code{numeric}. A year in the interval 0-17 (or
2000-2017). Default \code{0}.}
  \item{perc}{\code{numeric}.  Minimum percentage of canopy
closure per grid cell in \code{tc}. Default \code{80}.}
}
\details{Diverse \code{EBV} metrics are
supported: forest extents \code{'forest.ext'}
(\code{km^2}), fractal-dimension indices
\code{'frac.dim.index'} (dimensionless), and
other values in \code{\link{PatchStat}}. The
\code{GFC} are masked twice. The first mask
filters values which are equal or greater than
\code{perc}. The second mask subtracts those
values in the \code{'treecover2000'} raster
which correspond to values in the
\code{'lossyear'} raster, spanning from 2000 to
\code{year}. Surfaces of the median pixels are
computed supposing long/lat coordinates, see
\code{\link{area}}.}
\value{\code{ts}. EBV metric.}
\references{O'Connor, B., Secades, C.,
Penner, J., Sonnenschein, R., Skidmore, A.,
Burgess, N. D., & Hutton, J. M. (2015). Earth
observation as a tool for tracking progress
towards the Aichi Biodiversity Targets. Remote
sensing in ecology and conservation, 1(1),
19-28.}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
EBVmetric(NULL)
## Lets change mpio = NULL by a GADM unit: e.g., the municipality
## of 'Uribia' in Colombia and compute fractal dimensions from
## year 10 to year 17:
 \donttest{
 mpio <- 'Uribia'
 gadm <- FCPolygon(mpio, level = 2)
 smet <- EBVmetric(gadm, met = 'frac', year = 10:17)
 plot(smet)
 }
}
