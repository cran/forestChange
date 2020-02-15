barplot.EBVstats <- structure(function #barplot EBV Stats
###A barplot of \code{\link{EBVstats}} is printed.
(
    height, ##<< \code{list} of \code{EBVstats}.
    ... ##<< Additional arguments in \code{\link{barplot}}.
    

){
    if(!all(c('mean','sd')%in%names(height)))
        stop("Should provide at least: 'mean', and 'sd'")
    height. <- height$'mean'
    names  <- height$'layer'
    error  <-  height$'sd'
    maxLim <- 1.1* max(mapply(sum, height., error))
    ylim = c(0,maxLim)
    par(oma = c(0,1,0,0))

    bp <- barplot(height., names.arg = names,
                  ylim = ylim, mgp = c(2.8,1,0),
                  lwd = 2, cex.lab = 1.5, cex.axis = 1.5,
                  cex.names = 1.5,...)
    arrows(x0 = bp, y0 = height., y1 = height. + error,
           angle = 90, lwd = 1.3)
    arrows(x0 = bp, y0 = height., y1 = height. - error,
           angle = 90, lwd = 1.3)
### Plot of \code{EBVstats}.
} , ex=function(){
    height  <- list(mean = abs(rnorm(4)), sd = abs(rnorm(4)))
    class(height) <- 'EBVstats'
    barplot(height, main = '\nEBV Stats')

})
