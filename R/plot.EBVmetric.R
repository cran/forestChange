plot.EBVmetric <- structure(function#EBV-metric plot
###A plot of \code{\link{EBVmetric}} is printed.
(
    x, ##<<\code{\link{ts}}. Time series such as that produced by
       ##\code{\link{EBVmetric}}.
    ... ##<< further arguments in \code{\link{plot}} other than
        ##\code{cex.lab}, \code{type}, \code{xlab}, \code{ylab},
        ##\code{xaxt}, and \code{yaxt}.
){
    area. <- as.data.frame(x)
    area.[,'time'] <- as.numeric(time(x))
    ind <- 'x'
    area. <- na.omit(area.)
    rel <- formula(paste(ind, '~time', sep =""))
    par(oma = c(0,0,0,0))
    plot(rel, area.,
         axes = FALSE,
         type = 'n',
         xlab = '',
         ylab = '')
    xap <- (max(area.[,ind], na.rm = TRUE) - min(area.[,ind], na.rm = TRUE))/20
    xa. <- round((min(area.[,ind], na.rm = TRUE) - xap))
    yap <- pretty(area.[,ind])
    cx. <- 1.2
    plot(rel, area.,
         type = 'n',
         xaxt = 'n',
         yaxt = 'n',
         cex.lab = cx.,
         ## mgp = c(2,3,0),
         ...)
    axis(2, at = yap, cex.axis = cx.)
    fadd <- function(x, is.x = TRUE){
        if(is.x){
            rt <- c(min(x),x,max(x))
        }else{
            rt <- c(0,x,0)
        }
        return(rt)
    }
    polygon(fadd(area.$'time'), fadd(area.[,ind],F),
            border =  'grey70', col = 'grey80')
    axis(1, cex.axis = cx.)
    lines(rel, area., col = 'grey70', pch = 19, lwd = 2)
    points(rel, area., col = 'white', pch = 19, cex = 2)
    points(rel, area., col = 'black', pch = 19)
box(lwd = 2)
### \code{plot}.
} , ex=function(){
    ## Simulating an objec of class EBVmetric
    set.seed(1)
    areaKm2 <- 1800 - (rnorm(11))
    ats <- ts(areaKm2, start = 2000)
    class(ats) <- c('EBVmetric', class(ats)) 
    
    ## A plot of the 'EBVmetric' object
    plot(ats)
})
