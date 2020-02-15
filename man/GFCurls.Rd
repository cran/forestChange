\name{GFCurls}
\alias{GFCurls}
\title{URLs of GFC data}
\description{This function retrieves \code{URL} of Global Forest Change
(\code{GFC}) data.}
\usage{GFCurls(lyrs = c("treecover2000", "lossyear"), url)}
\arguments{
  \item{lyrs}{\code{character}. Name(s)
of the layers. Default
\code{'treecover2000'},
and \code{'lossyear'}}
  \item{url}{\code{character}.  Path to the \code{html} file containing
the files. Default \code{NULL} retrieves \code{URL}s stored
in the application programming interface of \code{GFC}, see
\code{References}.}
}

\value{\code{character} vector.}
\references{\href{http://earthenginepartners.appspot.com}{https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html}}
\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
 \donttest{
 gainLayers <- GFCurls(lyrs = 'gain')
 head(gainLayers)
 }
}
