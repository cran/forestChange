\name{plot.EBVmetric}
\alias{plot.EBVmetric}
\title{EBV-metric plot}
\description{A plot of \code{\link{EBVmetric}} is printed.}
\usage{\method{plot}{EBVmetric}(x, ...)}
\arguments{
  \item{x}{\code{\link{tibble}}. Data set of metrics such as
that produced by \code{\link{EBVmetric}}.}
  \item{...}{Further arguments not implemented here.}
}

\value{\code{plot}.}

\author{Wilson Lara <wilarhen@temple.edu>, Victor Gutierrez-Velez <victorhugo@temple.edu>}




\examples{
 \donttest{
 mpio <- 'Uribia'
 msk <- FCMask(mpio, year = 10:17)
 met <- EBVmetric(msk, what = 'lsm_l_frac_mn')
 plot(met)
 }
}
