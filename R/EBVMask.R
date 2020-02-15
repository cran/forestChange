EBVMask <- structure(function #EBV Mask
### This function combines EBV distribution maps with
### forest/non-forest masks from \code{\link{FCMask}}.
(
    pol = NULL, ##<<\code{SpatialPolygonsDataFrame}, \code{character}
               ##or \code{NULL}. User-defined polygon, the name of a
               ##\code{GADM}, or such a name plus its corresponding
               ##higher-level unit. If \code{NULL} then a list of
               ##\code{GADM} units is printed.
    path, ##<<\code{character}. File path(s) to the distribution maps
          ##maps.  (.tif). Compressed files (.zip) are extracted.
    int.patt = '[[:digit:]].tif', ##<<\code{character}. If files in
                                  ##\code{path} are compressed then
                                  ##common pattern in the extracted
                                  ##files.  Default
                                  ##\code{'[[:digit:]].tif'}.
    mc.cores = detectCores(), ##<<\code{numeric}. The number of cores.
    ... ##<< additional arguments in \code{\link{FCMask}}. This
        ##argument only works if \code{pol} is not an object produced
        ##by \code{\link{FCMask}}.
) {
    pol. <- pol
    if(inherits(pol, getOption('inh'))){
        pol <- FCMask(pol,...)# <-
        if(is.null(pol.))
            return(pol)}
    tifimag <- source2Env(zfe = path, int.patt = int.patt)
    tifimag <- stack(tifimag)
    extt <- extent(pol)
    prett <- projectExtent(pol, crs(tifimag)) 
    ars <- cropRaster(tifimag, extent(prett))
    ars. <- Filter(function(x)
        cellStats(x, sum)!=0, as.list(ars))
    names(ars.) <- names(stack(ars.))
    ars.. <- Map(function(x)
        f8(x, 1), ars.)
    simp. <- projectRaster(stack(ars..),
                           pol[[1L]], method = 'ngb')
    mk <- mask(simp., raster::subset(pol, nlayers(pol)))
    mkr <- stack(Filter(function(x)
        cellStats(x, sum)!=0, as.list(mk)))
    mkr <- stack(Filter(function(x)
        cellStats(x, sum)!=0, as.list(mk)))
    return(mkr)
### \code{RasterStack}, set of \code{GADM} units, or \code{NULL}.
} , ex=function() {
    mun  <- EBVMask(NULL)
})
