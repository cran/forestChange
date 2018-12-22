FCMosaic <- structure(function #Forest-Change Mosaic
### This function tests whether two adjacent layers of \code{GFC} can
### be bounded together using partial matching over the names of the
### layers. If this is possible then \code{\link{mosaic}} is
### implemented.
                      ##details<< The function is implemented by
                      ##\code{\link{FCPolygon}} to cut \code{GFC}
                      ##data.
(
    rst = NULL, ##<<\code{list} or \code{NULL}. List of \code{GFC}
                ##layers. If \code{NULL} then the other arguments are
                ##ignored and the function returns \code{NULL}.
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Strings
                                          ##matching layers in a
                                          ##google api used to
                                          ##download \code{GFC}
                                          ##layers. Default
                                          ##\code{'treecover2000'},
                                          ##and \code{'lossyear'}.
    multicore = TRUE ##<<\code{logical}. Use parallel
                     ##execution. Default \code{TRUE}. Ignored in
                     ##Windows machines.
) {
    if(is.null(rst))
        return(NULL)
    if(length(rst) == length(lyrs)){
        return(stack(rst))
    }
    nmr <- lapply(rst,function(x)
        names(x))
    spr <- lapply(lyrs,function(x)
        grepl(x, nmr))
    spr1 <- lapply(spr, function(x)
        rst[x])
    arbn <- list(FUN = function(x, fun = mean)
        do.call('mosaic',
                c(x, list(fun = mean))),
        x = spr1,
        SIMPLIFY = FALSE)
    fprll <- 'mapply'
    if(multicore){
        arbn <- c(arbn,mc.cores = detectCores())
        fprll <- 'mcmapply'
    }
    print('FCMosaic: Mosaicing is required ...')
    msc <- do.call(fprll, arbn)
    stc <- stack(msc)
    names(stc) <- lyrs
    return(stc)
### \code{list} of rasters.
} , ex=function() {
## Printing NULL output:    
FCMosaic(NULL)
})
