\name{HansenUrltoExtent}
\alias{HansenUrltoExtent}
\title{Extents in GFC links}
\description{This function can extract extents of Global Forest Change data
(\code{GFC}) using a corresponding \code{URL}.}
\usage{
HansenUrltoExtent(x, path. = "[[:digit:]]{1,3}[N|S|E|W]")
}
\arguments{
  \item{x}{\code{character}. \code{URL} to the \code{GFC} such as any
of these produced by \code{\link{GFCurls}}.}
  \item{path.}{\code{character}. Pattern
in the \code{URL} to extract
the extent. Default extracts
the 3 digits nearest to any
of the cardinal
directions:\code{N},
\code{S}, \code{E}, or
\code{W}.}
}
\details{The function is implemented by
\code{\link{FCPolygon}}.}
\value{extent.}
\references{\href{http://earthenginepartners.appspot.com}{http://earthenginepartners.appspot.com/science-2013-global-forest}}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
urtt <- 'https://storage.googleapis.com/earthenginepartners-hansen/
GFC-2017-v1.5/Hansen_GFC-2017-v1.5_treecover2000_10N_010E.tif'
HansenUrltoExtent(urtt)
    
}
