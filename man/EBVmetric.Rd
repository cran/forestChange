\name{EBVmetric}
\alias{EBVmetric}
\title{EBV metric}
\description{This function formats \code{GFC} and computes structural metrics
of Essential Biodiversity Variables (\code{EBV} metrics):
forest-cover areas and landscape metrics, in predefined polygons
or GADM.}
\usage{EBVmetric(pol, what = "lsm_l_tafc", mc.cores = detectCores(), 
    ...)}
\arguments{
  \item{pol}{\code{RasterStack}.Stack such as these produced by
\code{\link{FCMask}}.}
  \item{what}{\code{character}. The metrics. These
include \code{'lsm_l_tafc'} to compute
total forest-cover areas (ha) and other
landscape metrics in
\code{\link{calculate_lsm}}. Default
(\code{'lsm_l_tafc'}).}
  \item{mc.cores}{\code{numeric}. The number of cores,
see \code{\link{mclapply}}.}
  \item{\dots}{additional arguments in \code{\link{calculate_lsm}}.}
}
\details{Coordinate system of the spatial
units should be \code{ UTM}. Metrics other
than \code{'lsm_l_tafc'} are calculated
implementing \code{\link{calculate_lsm}}.}
\value{\code{tibble}.}
\references{O'Connor, B., Secades, C.,
Penner, J., Sonnenschein, R., Skidmore, A.,
Burgess, N. D., & Hutton, J. M. (2015). Earth
observation as a tool for tracking progress
towards the Aichi Biodiversity Targets. Remote
sensing in ecology and conservation, 1(1),
19-28.}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
 \donttest{
 mpio <- 'Uribia'
 msk <- FCMask(mpio, year = 10:17)
 met <- EBVmetric(msk, what = 'lsm_l_frac_mn')
 plot(met)
 }
}
