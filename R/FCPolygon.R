FCPolygon <- structure(function #Forest-Cover Polygon
### This function can retrieve and crop layers of Global Forest Change
### (\code{GFC}) using  polygon geometries (i.e., GADM).
                       ##details<< The \code{GADM} are imported using
                       ##the in-package \code{\link{getGADM}}.
                       ##Links to the data sets are obtained using
                       ##the in-package
                       ##\code{\link{GFCurls}}. Geographic extents
                       ##in both the \code{GADM} and the \code{GFC}
                       ##are intersected implementing
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
                       ##package implements parallel execution,
                       ##see \code{\link{parallel}} package.
                       ##references<< Hansen, M. C., Potapov,
                       ##P. V., Moore, R., Hancher, M., Turubanova,
                       ##S. A. A., Tyukavina, A., ... & Kommareddy,
                       ##A. (2013). High-resolution global maps of
                       ##21st-century forest cover change. science,
                       ##342(6160), 850-853.
(
    pol = NULL, ##<<\code{SpatialPolygonsDataFrame}, or
                ##\code{character}. Polygon geometry, the name of a
                ##\code{GADM}, or such a name plus its corresponding
                ##higher-level unit. If \code{NULL} then a list of
                ##\code{GADM} units is printed, see
                ##\code{\link{getGADM}}.
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Vector
                                          ##of strings matching layer
                                          ##names in the \code{GFC}.
                                          ##Defaults
                                          ##\code{'treecover2000'} and
                                          ##\code{'lossyear'}.
    path, ##<<\code{character}.Location of a directory with the
          ##\code{GFC}. This argument overrides the action of \code{url}.
    url, ##<<\code{character}.  Web resource with text files
         ##containing lists of \code{URL}s for the \code{GFC}
         ##layers. If missing then data from the application
         ##programming interface of \code{GFC} is retrieved, see
    ##\code{\link{GFCurls}}.
    pr.utm = TRUE, ##<<\code{logical}. Project to UTM crs.
    mc.cores = detectCores(), ##<<\code{numeric}. The number of cores,
                              ##see \code{\link{mclapply}}.
    ... ##<<Additional arguments in \code{\link{getGADM}}.
) {
    pol. <- pol
    if(inherits(pol, getOption('inh')[3:4])){
        pol <- getGADM(pol,...)# <-
        if(is.null(pol.))
            return(pol)}
    fils <- paste(lyrs, collapse = '|')
    fprll <- getOption('fapp')
    if(missing(path)){
        urt. <- GFCurls(lyrs, url)# <-
        td <- tempdir()
        inters <- sapply(urt., function(x)
            raster::intersect(HansenUrltoExtent(x), # <-
                              extent(pol)))
        do.inters <- unlist(lapply(
            inters, function(x)!is.null(x)))
        urls <- urt.[do.inters]
        url <- urls[grepl(fils,urls)]
        fl <- file.path(td, basename(url))
        right <- sapply(lyrs, function(x)grep(x, fl))
        fl <- fl[right]
        if(!getOption('isWin')){
            marg[['mc.cores']] <- mc.cores
        }
        marg. <- c(list(FUN = function(x,y, mode = 'wb')
            download.file(x,y, mode = 'wb'),
            x = url,
            y = fl), marg)
        if(!all(basename(url)%in%dir(td))){
            outp <- do.call(fprll, marg.)}
    }
    if(!missing(path)){
        fils <- paste(lyrs, collapse = '|')
        drp <- dir(path)
        grpath <- drp[grepl(fils,drp)]
        fl <- file.path(path, grpath)
        right <- sapply(lyrs, function(x)grep(x, fl))
        fl <- fl[right]
    }
    rst <- lapply(fl,function(x)raster(x))
    if(extent(rst[[1L]]) != extent(pol)){
        marg. <- c(list(FUN = function(x,y)
            cropRaster(x,y),
            x = rst,
            MoreArgs = list(y = pol)), marg)
        print('Cutting layers ...')
        rst. <- do.call(fprll, marg.)
        rst <- FCMosaic(rst., lyrs, mc.cores = mc.cores) # <-
    }
    if(pr.utm){
        long2UTM <- function(long) {
            (floor((long + 180)/6) %% 60) + 1
        }
        l2u <- long2UTM(extent(rst)[1L])
        sr <- paste("+proj=utm +zone=", l2u," +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep ='')
        polpr <- projectExtent(subset(rst, 1), crs = sr)
        rst <- projectRaster(rst, polpr)
    }
    ## rst <- stack(rst)
    names(rst) <- lyrs
    return(rst)
### \code{RasterStack}, or set of \code{GADM} units.
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
