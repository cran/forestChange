\name{GFCurls}
\alias{GFCurls}
\title{URLs of GFC}
\description{This function can find lists of \code{URL} necessary to download Global
Forest Change data (\code{GFC}).}
\usage{GFCurls(lyrs = c("treecover2000", "lossyear"), gglapi = NULL, 
    ext = ".txt")}
\arguments{
  \item{lyrs}{\code{character}. Portion
of the \code{URL}s matching
names of \code{GFC}
layers. Default
\code{'treecover2000'},
and \code{'lossyear'}}
  \item{gglapi}{\code{character}.  Portion of an \code{URL}
which is common to the set of \code{URL}s to be
retrieved. If \code{NULL} then an application
programming interface of google is used, see
\code{References}.}
  \item{ext}{\code{logical}. Extension of the file
containing the links. Default \code{'txt'}}
}
\details{The function is implemented by
\code{\link{FCPolygon}}.}
\value{\code{character} vector.}
\references{\href{http://earthenginepartners.appspot.com}{http://earthenginepartners.appspot.com/science-2013-global-forest}}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
gainLayers <- GFCurls(lyrs = 'gain')
head(gainLayers)    
}
