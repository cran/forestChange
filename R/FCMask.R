FCMask <- structure(function #Forest-Change Mask
### This function can format \code{GFC} into forest-distribution
### regions of interest over time.
(
    pol = NULL, ##<<\code{RasterStack};
                    ##\code{SpatialPolygonsDataFrame}; \code{Extent};
                    ##\code{character}, or \code{NULL}. Spatial object
                    ##such as any of these produced by \code{getGADM}
                    ##or by \code{FCPolygon}. If \code{NULL} then a
                    ##list of unit names is printed, see
                    ##\code{getGADM}.
    year = 1:2, ##<<\code{numeric}. Years between 0 and 18 (or between
                ##2000 and 2018).
    cummask = TRUE, ##<<\code{logical}. Compute cumulative masks
                    ##instead of discrete masks. Default \code{TRUE}.
    deforest = FALSE, ##<<\code{logical}. Process non-forest areas
                     ##instead of forest areas. Default \code{FALSE}.
    perc = 80:100, ##<< \code{numeric}.  Percentage of canopy
                   ##closure. Default \code{80:100}. This argument is
                   ##ignored if \code{deforest = TRUE}.
    mc.cores = detectCores(), ##<<\code{numeric}. The number of cores,
                              ##see \code{\link{mclapply}}.
    ... ##<<Additional arguments in \code{\link{FCPolygon}}.
) {
    pol. <- pol
    if(inherits(pol, getOption('inh'))){
        pol <- FCPolygon(pol, mc.cores = mc.cores, ...)# <-
        if(is.null(pol.))
            return(pol)}
    lyi <- getOption('trls')%in%names(pol)
    if(!all(lyi)){
        stop(paste(getOption('miss'),
              names(pol)[lyi],'; ', sep = ''))
    }
        year. <- year <- scaleYear(year)
    if(any(year == 0)){
    year. <- year[!year%in%0] 
    }
    lss <- raster::subset(pol, getOption('trls')[2L])
    mx <- sapply(c('min', 'max'), function(x)
        cellStats(lss, stat = x))
    intr <- seq(mx[1L],mx[2L],1)
    if(any(!year%in%intr)){
        yrlk <- year[!year%in%intr]
            stop(paste(getOption('miss'),
                       yrlk,'; ', sep = ''))
    }
    fprll <- getOption('fapp')
    if(!getOption('isWin')){
        marg[['mc.cores']] <- mc.cores
    }
    marg. <- c(list(FUN = function(x)
        f8(lss, x), x = year.), marg)

    if(cummask){
        marg.[['FUN']] <- function(x)
            f8(lss, 1:x)}
    trc <- raster::subset(pol, getOption('trls')[1L])
    w <- do.call(fprll, marg.)
    if(any(year == 0)){
        X0 <- trc
        raster::values(X0) <- NA
        w <- c(X0, w)}
    w <- brick(w)
    tr <- f8(trc, perc)
    w <- mask(tr, w, inverse = !deforest)
    names(w) <- year
    ## w <- raster::as.list(w)
    ## names(w) <- year
    ## class(w) <- append('FCMask', class(w))
    return(w)
### \code{RasterBrick} of forest/non-forest masks over time. or
### \code{character} vector with \code{GADM}.
} , ex=function() {
    ## A mask of GFC layers in the municipality of 'Uribia' is computed:
    ## \donttest{
    ## UribiaMask <- FCMask('Uribia')
    ## plot(UribiaMask)
    ## }
})
