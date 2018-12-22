\name{getGADM}
\alias{getGADM}
\title{Get Geographic Adminitrative Unit}
\description{This function is a wrapper of \code{\link{getData}} used to import
levels in Geographic Administrative Units (\code{GADM}).}
\usage{getGADM(unit.nm = NULL, level = 2, country = "COL")}
\arguments{
  \item{unit.nm}{\code{character} or \code{NULL}. A name in the
administrative units (e.g. municipalities), or
the name of the unit plus its corresponding
higher-level unit (e.g. department/state). If
\code{NULL} then a list of unit names
corresponding to \code{'level'} is printed.}
  \item{level}{\code{numeric}. A number between zero and two,
indicating any of the levels of administrative subdivisions in \code{GADM}:
\code{0=country}, \code{1=first level of
subdivision}, and \code{2=second level of
subdivision}).}
  \item{country}{\code{character}. \code{ISO} code specifying a
country. Default \code{'COL'}}
}

\value{List of rasters.}
\references{\href{https://gadm.org/}{https://gadm.org/}}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
## Printing municipalities of Colombia:    
muni <- getGADM()
head(muni)
}
