\name{barplot.EBVstats}
\alias{barplot.EBVstats}
\title{barplot EBV Stats}
\description{A barplot of \code{\link{EBVstats}} is printed.}
\usage{\method{barplot}{EBVstats}(height, ...)}
\arguments{
  \item{height}{\code{list} of \code{EBVstats}.}
  \item{\dots}{Additional arguments in \code{\link{barplot}}.}
}

\value{Plot of \code{EBVstats}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
height  <- list(mean = abs(rnorm(4)), sd = abs(rnorm(4)))
class(height) <- 'EBVstats'
barplot(height, main = '\nEBV Stats')

}
