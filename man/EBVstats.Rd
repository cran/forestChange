\name{EBVstats}
\alias{EBVstats}
\title{EBV Stats}
\description{This function is a wrapper of \code{cellStats} to computes
statistics for \code{\link{CCPolygon}} objects.}
\usage{EBVstats(ccp, stats, mc.cores = detectCores(), ...)}
\arguments{
  \item{ccp}{\code{RasterStack} or \code{NULL}. Stack such as that
produced by \code{\link{CCPolygon}}. If \code{NULL} then
\code{NULL} is returned.}
  \item{stats}{\code{character} vector of stats defined in
\code{\link{cellStats}}. If missing then \code{'mean'},
\code{'sd'} and \code{'max'} are computed.}
  \item{mc.cores}{\code{numeric}. The number of cores,
see \code{\link{mclapply}}.}
  \item{\dots}{Additional arguments in \code{cellStats}}
}

\value{\code{list} of \code{EBVstats}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
EBVstats(NULL)
}
