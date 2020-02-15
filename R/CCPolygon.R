CCPolygon <- structure(function #Canopy-cover polygon
### This function computes regions of interest across \code{GFC} areas
### for tree-cover data (\code{GFC30TC}).
                       ##references<< Sexton, J. O., Song, X. P.,
                       ##Feng, M., Noojipady, P., Anand, A., Huang,
                       ##C., ... & Townshend, J. R. (2013). Global,
                       ##30-m resolution continuous fields of tree
                       ##cover: Landsat-based rescaling of MODIS
                       ##vegetation continuous fields with lidar-based
                       ##estimates of error. International Journal of
                       ##Digital Earth, 6(5), 427-448.
(
    pol = NULL, ##<<\code{SpatialPolygonsDataFrame}, \code{character}
                ##or \code{NULL}. Polygon geometry, \code{GADM}, stack
                ##such as tha produced by \code{FCPolygon}, or
                ##\code{NULL}. If \code{NULL} then a list of
                ##\code{GADM} units is printed.
    path, ##<<\code{character}. File path(s) to \code{GFC30TC} layers
          ##(.tif), or to zip archives containing \code{GFC30TC}.
    int.patt = '[[:digit:]].tif', ##<<\code{character}. If files in
                                  ##\code{path} are compressed then
                                  ##common pattern in the extracted
                                  ##files.  Default
                                  ##\code{'[[:digit:]].tif'}.
    mc.cores = detectCores(), ##<<\code{logical}. Use parallel
                             ##execution. Default \code{TRUE}. Ignored
                             ##in Windows machines.
    ... ##<<Additional arguments in \code{\link{FCMask}}.
) {
    pol. <- pol
    if(inherits(pol, getOption('inh'))){
        pol <- FCMask(pol,...)# <-
        if(is.null(pol.))
            return(pol)}
    
    fprll <- getOption('fapp')
    if(!getOption('isWin'))
        marg[['mc.cores']] <- mc.cores
    rst <- source2Env(zfe = path, int.patt = int.patt)
    ## rsti <- Map(function(x)
    ##     projectExtent(x, crs(pol)), as.list(rst))
    ## system.time(
    ##     doesint <- Map(function(x)
    ##     !is.null(intersect(extent(pol), x)), rsti)
    ## )
    ##   user  system elapsed 
    ## 58.334  17.610  75.953 
    ## doint <- unlist(doesint)
    ## rst <- rst[doint]
    extt <- extent(pol)
    prett <- projectExtent(pol, crs(rst[[1L]])) 
    ars <- Map(function(x)
        cropRaster(x, extent(prett)),rst)
    ars. <- Filter(function(x)
        cellStats(x, sum)!=0, raster::as.list(ars))
    names(ars.) <- lapply(ars., 'names')
    nmss <- names(ars.)
    spl <- strsplit(nmss,'_')
    oldw <- getOption("warn")
    options(warn = -1)
    yrss <- unique(sapply(spl, function(x)
        na.omit(as.numeric(x))))
    options(warn = oldw)
    lstoMerge <- function(time, ars., extt){
        nmss <- names(ars.)
        yr.. <- nmss[grepl(time, nmss)]
        lyrsel <- ars.[yr..]
        lyrsel <- lapply(lyrsel, function(x)
            crop(x, round(extent(extt))))
        names(lyrsel)[1:2]  <- c('x','y')
        mrg <- do.call('merge', lyrsel)
        names(mrg) <- time
        return(mrg)}
    rst. <- Map(function(x)
        lstoMerge(x, ars., extt), x = yrss)
    srss <- stack(rst.)
    mssl <- raster::subset(pol, nlayers(pol))
    system.time(
        prr <- projectRaster(mssl, srss)
    )
    msks <- mask(srss, prr)
    msksp <- Map(function(x)
        f16(x, a = 1:100), raster::as.list(msks))
    msksp <- stack(msksp)
    names(msksp) <- yrss
    return(msksp)
### \code{RasterStack}, set of \code{GADM} units, or \code{NULL}.
} , ex=function() {
    mun  <- CCPolygon(NULL)
})
