EBVstats <- structure(function #EBV Stats
### This function is a wrapper of \code{cellStats} to computes
### statistics for \code{\link{CCPolygon}} objects.
(
    ccp, ##<< \code{RasterStack} or \code{NULL}. Stack such as that
         ##produced by \code{\link{CCPolygon}}. If \code{NULL} then
         ##\code{NULL} is returned.
    stats, ##<<\code{character} vector of stats defined in
           ##\code{\link{cellStats}}. If missing then \code{'mean'},
           ##\code{'sd'} and \code{'max'} are computed.
    mc.cores = detectCores(), ##<<\code{numeric}. The number of cores,
                              ##see \code{\link{mclapply}}.
    ... ##<<Additional arguments in \code{cellStats}

){
    if(is.null(ccp))
        return(NULL)
    if(missing(stats))
        stats <- c('min','mean', 'max', 'sd', 'skew', 'rms')
    tyr <- as.numeric(
        sub("\\D+","", names(ccp)))
    if(all(is.na(tyr)))
        tyr <- names(ccp)
##     fprll <- getOption('fapp')
##         marg. <- c(list(FUN = function(x,y)
##             raster::cellStats(x,stat = y),
##             y = stats, 
##             MoreArgs = list(x = ccp)), marg)

        
##         sts <- do.call(fprll, marg.)
        sts <- Map(function(x,...)
        raster::cellStats(ccp, x,...),
        stats,...)
        sts <- t(do.call('rbind', sts))
        sts <- cbind(layer = tyr, sts)
        sts <- as_tibble(sts)
        ## rownames(sts) <- tyr
        class(sts) <- c('EBVstats', class(sts))
        return(sts)
### \code{list} of \code{EBVstats}.
} , ex=function(){
    EBVstats(NULL)
})
