FCMosaic <- structure(function #Forest-Change Mosaic
### This function tests whether adjacent layers of \code{GFC} can be
### bounded together using partial matching over the layer names. If
### it is possible then \code{\link{mosaic}} is implemented.
                      ##details<< The function is implemented by
                      ##\code{\link{FCPolygon}} to cut \code{GFC}
                      ##data.
(
    rst = NULL, ##<<\code{list} or \code{NULL}. List of \code{GFC}
                ##layers. If \code{NULL} then other arguments are
                ##ignored and the function returns \code{NULL}.
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Vector
                                          ##of strings matching layer
                                          ##names in \code{GFC}
                                          ##data. Defaults
                                          ##\code{'treecover2000'} and
                                          ##\code{'lossyear'}.
    mc.cores = detectCores() ##<<\code{logical}. Use parallel
                             ##execution. Default \code{TRUE}. Ignored
                             ##in Windows machines.
) {
    if(is.null(rst))
        return(NULL)
    if(length(rst) == length(lyrs)){
        return(stack(rst))
    }
    rst <- raster::as.list(rst)
    nmr <- lapply(rst,function(x)
        names(x))
    spr <- lapply(lyrs,function(x)
        grepl(x, nmr))
    spr1 <- lapply(spr, function(x)
        rst[x])
    fprll <- getOption('fapp')
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores
    }
    names(spr1)[1:2] <- c('x','y')
    marg. <- c(list(FUN = function(x)
        do.call('merge', x), x = spr1),marg)
    ## marg. <- c(list(FUN = function(x, fun = mean)
    ##     do.call('mosaic',
    ##             c(x, list(fun = mean))),
    ##     x = spr1),marg)

    print('FCMosaic: Mosaicing is required ...')
    msc <- do.call(fprll, marg.)
    stc <- stack(msc)
    names(stc) <- lyrs
    return(stc)
### \code{list} of rasters.
} , ex=function() {
## Printing NULL output:    
FCMosaic(NULL)
})
