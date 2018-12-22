\name{plot.EBVmetric}
\alias{plot.EBVmetric}
\title{EBV-metric plot}
\description{A plot of \code{\link{EBVmetric}} is printed.}
\usage{\method{plot}{EBVmetric}(x, ...)}
\arguments{
  \item{x}{\code{\link{ts}}. Time series such as that produced by
\code{\link{EBVmetric}}.}
  \item{\dots}{further arguments in \code{\link{plot}} other than
\code{cex.lab}, \code{type}, \code{xlab}, \code{ylab},
\code{xaxt}, and \code{yaxt}.}
}

\value{\code{plot}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
## Simulating an objec of class EBVmetric
set.seed(1)
areaKm2 <- 1800 - (rnorm(11))
ats <- ts(areaKm2, start = 2000)
class(ats) <- c('EBVmetric', class(ats)) 

## A plot of the 'EBVmetric' object
plot(ats)
}
