EBVmetric <- structure(function #EBV metric
### This function can compute metrics of two Essential Biodiversity
### Variables (\code{EBV} metrics): forest extents and forest
### fragmentation indices using Global Forest Change data
### (\code{GFC}).
                     ##details<< Diverse \code{EBV} metrics are
                     ##supported: forest extents \code{'forest.ext'}
                     ##(\code{km^2}), fractal-dimension indices
                     ##\code{'frac.dim.index'} (dimensionless), and
                     ##other values in \code{\link{PatchStat}}. The
                     ##\code{GFC} are masked twice. The first mask
                     ##filters values which are equal or greater than
                     ##\code{perc}. The second mask subtracts those
                     ##values in the \code{'treecover2000'} raster
                     ##which correspond to values in the
                     ##\code{'lossyear'} raster, spanning from 2000 to
                     ##\code{year}. Surfaces of the median pixels are
                     ##computed supposing long/lat coordinates, see
                       ##\code{\link{area}}.
                     ##references<<O'Connor, B., Secades, C.,
                     ##Penner, J., Sonnenschein, R., Skidmore, A.,
                     ##Burgess, N. D., & Hutton, J. M. (2015). Earth
                     ##observation as a tool for tracking progress
                     ##towards the Aichi Biodiversity Targets. Remote
                     ##sensing in ecology and conservation, 1(1),
                     ##19-28.
(
    tc, ##<<\code{RasterStack}. \code{GFC} layers such as that
        ##produced by \code{\link{FCPolygon}}.
    met = 'forest.ext', ##<<\code{character}. Name of a \code{EBV}
                        ##metric, see \code{Details} section. Partial
                        ##matching is supported. Default
                        ##\code{'forest.ext'}.
    year = 0, ##<<\code{numeric}. A year in the interval 0-17 (or
              ##2000-2017). Default \code{0}.
    perc = 80 ##<< \code{numeric}.  Minimum percentage of canopy
              ##closure per grid cell in \code{tc}. Default \code{80}.
) {
    if(is.null(tc))
        return(NULL)
    recTable <- function(thr = 0,
                         oneFirst = TRUE){ 
        m <- diag(c(-1,1) * Inf)
        then <- c(1, NA)
        if(thr != 0)
            m[m == 0] <- thr
        if(!oneFirst)
            then <- rev(then)
        mat <- cbind(m, then)
        return(mat)}
    scaleYr <- function(year){
            year <- as.vector(scale(year,2E3,1))
            return(year)}
    fa <- function(x){
        ar <- area(x, na.rm = TRUE)
        ar <- na.omit(values(ar))
        ar. <- length(ar)*median(ar)
        return(ar.)}
    trc <- tc[[names(tc)[grepl('tree', names(tc))]]]
    lss <- tc[[names(tc)[grepl('loss', names(tc))]]]
    if(all(year >= 2E3))
    year <- scaleYr(year)
    year. <- year
    myr <- max(na.omit(values(lss)))
    if(any(!year%in%c(0:myr))){
        year. <- year.[year. > myr]
        yrw <- paste(' Year ', year.,
                     ' has no GFC records.',
                     sep ='')
        warning(yrw)
    year <- year[year <= myr]}
    mpr <- recTable(perc, FALSE)
    fsl <- reclassify(trc, mpr)
    if(all(year == 0)){
        if(met%in%'forest.ext')
            area.. <- ts(fa(fsl), start = 0)
        if(!met%in%'forest.ext')
            area.. <- ts(c(lfd(fsl, it = met)), start = 0)
        return(area..)}
    msks <- Map(function(x)
        mask(fsl, lss, maskvalue = x, inverse = TRUE),
        year)
    lfd <- function(x, it = 'frac'){
        are <- area(x, na.rm = TRUE)
        are <- na.omit(getValues(are))
        pst. <- SDMTools::PatchStat(x,
                              cellsize = sqrt(median(are)*1E6),
                              latlon = TRUE)
        it. <- names(pst.)[
            grepl(it, names(pst.))]
        dfr <- pst.[it.]
        names(dfr) <- NULL        
        return(dfr)}
    yr <- seq_len(year[length(year)])
    msks <- list(fsl)
    msks[2:(length(yr) + 1)] <- Map(function(x)
        mask(fsl, lss, maskvalue = x, inverse = FALSE),
        yr)
    msks. <- Reduce('*', msks, accumulate = TRUE)
    names(msks.) <- c(0,yr)
    if(met%in%'forest.ext'){
    areas <- mapply(function(x)
        fa(x), msks., SIMPLIFY = TRUE)
    }else{
        areas <- mapply(function(x)
            lfd(x, it = met), msks., SIMPLIFY = TRUE)}
        if(inherits(areas, 'list')){
            areas <- unlist(areas)}
    if(inherits(areas,'numeric'))
            areas <- areas[names(areas)%in%year]
        if(inherits(areas, 'matrix'))
            areas <- t(areas[,colnames(areas)%in%year])
    area. <- ts(areas, start = year[1L])
        class(area.) <- c('EBVmetric', class(area.))
        return(area.)
### \code{ts}. EBV metric.
} , ex=function() {
    EBVmetric(NULL)
    ## Lets change mpio = NULL by a GADM unit: e.g., the municipality
    ## of 'Uribia' in Colombia and compute fractal dimensions from
    ## year 10 to year 17:
    ## \donttest{
    mpio <- 'Uribia'
    gadm <- FCPolygon(mpio, level = 2)
    smet <- EBVmetric(gadm, met = 'frac', year = 10:17)
    plot(smet)
    ## }
})
