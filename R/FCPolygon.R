FCPolygon <- structure(function #Forest-Cover Polygon
### This function can crop layers of Global Forest Change (\code{GFC})
### using either Geographic Administrative Units (\code{GADM}) or
### other user-defined polygons.
                       ##details<< The \code{GADM} are imported using
                       ##the in-package \code{\link{getGADM}}.
                       ##Links to the data sets are obtained using
                       ##the in-package
                       ##\code{\link{GFCurls}}. Geographic extents
                       ##in both the \code{GADM} and the \code{GFC}
                       ##are intersected using the package function
                       ##\code{\link{HansenUrltoExtent}}. Common
                       ##areas between \code{GFC} and \code{GADM}
                       ##are cropped using two functions of the
                       ##\code{\link{raster}} package:
                       ##\code{\link{crop}} and
                       ##\code{\link{rasterize}}. Depending on
                       ##localization of the \code{GADM} unit,
                       ##several \code{GFC} layers by data type
                       ##might be required. This is done
                       ##implementing the in-package
                       ##\code{\link{FCMosaic}}. This function
                       ##could be memory demanding if the extents
                       ##of the polygons used to cut the \code{GFC}
                       ##are big (30,000 km^2). For these cases,
                       ##machines with RAM of 8 GB or greater should
                       ##be used. In unix-alike systems, the
                       ##package can implement parallel execution,
                       ##see \code{\link{parallel}} package.
                       ##references<< Hansen, M. C., Potapov,
                       ##P. V., Moore, R., Hancher, M., Turubanova,
                       ##S. A. A., Tyukavina, A., ... & Kommareddy,
                       ##A. (2013). High-resolution global maps of
                       ##21st-century forest cover change. science,
                       ##342(6160), 850-853.
(
    pol = NULL, ##<<\code{SpatialPolygonsDataFrame}, \code{character}
                ##or \code{NULL}. A spatial-data polygon, the name of
                ##a \code{GADM}, or such a name plus its corresponding
                ##higher-level unit. If \code{NULL} then a list of
                ##\code{GADM} units is printed.
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Vector
                                          ##of strings matching layer
                                          ##names in the \code{GFC}
                                          ##data. Defaults
                                          ##\code{'treecover2000'} and
                                          ##\code{'lossyear'}.
        url, ##<<\code{character}.  Path to the \code{html} file
               ##containing the files. If missing then data from the
               ##application programming interface of \code{GFC} is
               ##retrieved, see \code{\link{GFCurls}}.
    multicore = TRUE, ##<<\code{logical}. Use parallel
                      ##execution. Default TRUE. This is ignored in
                      ##Windows machines.
    ... ##<<Additional arguments in \code{\link{getGADM}} other than
        ##\code{'unit.nm'}. These could be \code{'level'} and/or
        ##\code{'country'}.
) {
    
    cropRaster <- function(rst, br){
        crp <- crop(rst, br)
        msk <- rasterize(br, crp, mask = TRUE)
        return(msk)
    }
    
    adm <- pol
    if(is.null(pol) | is.character(pol)){
        adm <- getGADM(pol,...)# <-
        if(is.null(pol))
            return(adm)}
    if(missing(url))
        url  <- NULL
    urt. <- GFCurls(lyrs, url)# <-
    td <- tempdir()
    inters <- sapply(urt., function(x)
        raster::intersect(HansenUrltoExtent(x), # <-
                          extent(adm)))
    do.inters <- unlist(lapply(
        inters, function(x)!is.null(x)))
    urls <- urt.[do.inters]
    layers <- attr(urt.,'lyrs')
    sep. <- '/' 
    if(!grepl(sep., urls[1]))
        sep. <- '\\' 
        do.inters <- unlist(lapply(
            inters, function(x)!is.null(x)))
        urls <- urt.[do.inters]
        coo2 <- grep(
            paste(layers, collapse = '|'), urls)
        url <- urls[coo2]
        fl <- paste(td, basename(url), sep = sep.)
        numCores <- detectCores()
        fprll <- 'mapply'
        marg <- list(FUN = function(x,y, mode = 'wb')
            download.file(x,y, mode = 'wb'),
            x = url,
            y = fl,
            SIMPLIFY = FALSE)
        if(Sys.info()['sysname']%in%'Windows')
            multicore <- FALSE
            if(multicore){
                fprll <- 'mcmapply'
                marg <- c(marg, mc.cores = detectCores())}
            if(!all(basename(url)%in%dir(td)))
                outp <- do.call(fprll, marg)
                rst <- lapply(fl,function(x)raster(x))
                sev. <- length(layers) < length(rst)
                marg. <- list(FUN = function(x,y)
                    cropRaster(x,y),
                    x = rst,
                    MoreArgs = list(y = adm),
                    SIMPLIFY = FALSE)
                if(multicore)
                    marg. <- c(marg., mc.cores = detectCores())
                    print('Cutting layers ...')
                    rst. <- do.call(fprll, marg.)
                    for(i in 1:length(rst))
                        names(rst.[[i]]) <- names(rst[[i]])
                        rst.. <- FCMosaic(rst., lyrs, multicore) # <-
                        unit. <- 'Polygon'
                        if(is.null(pol) | is.character(pol)){
                            unit. <- pol
                        }
                        attributes(rst..) <- append(attributes(rst..),
                                                    list(unit.nm = unit.))
                        return(rst..)
### list of rasters or set of \code{GADM} units
} , ex=function() {
    ## A list of departments of Colombia is printed:
    ## \donttest{
    ## dep <- FCPolygon(level = 1)
    ## head(dep)
    ## }
    ## Two adjacent layers of GFC must be bounded together before cropping
    ## the GFC data using the boundaries of the the municipality of
    ## 'Cumaribo' in Colombia. This is automatically developed by
    ## FCPolygon:
    ## \donttest{
    ## cumariboArea <- FCPolygon(pol = 'Cumaribo')
    ## }
    ## The name 'Mosquera' matchs two municipalities of Colombia. A
    ## corresponding department should be specified in the argument 'pol'
    ## of FCPolygon:
    ## \donttest{
    ## mosquera <- FCPolygon('Mosquera')
    ## mosqueraNarinho <- FCPolygon(pol = c('Mosquera','Narino'))
    ## }
})
