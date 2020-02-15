## Internal utility functions used by forestChange

cropRaster <- function(rst, br){
    crp <- crop(rst, br)
    msk <- rasterize(br, crp, mask = TRUE)
    return(msk)}

crwrs <- function(rst, yr., mn.){
    yr.. <- names(rst)[grepl(yr., names(rst))]
    lyrsel <- rst[yr..]
    lyrsel <- lapply(lyrsel, function(x)
        crop(x, round(extent(mn.))))
    names(lyrsel)[1:2]  <- c('x','y')
    mrg <- do.call('merge', lyrsel)
    mrg <- mask(mrg, mn.)
    return(mrg)}

## fa <- function(x){
##     ar <- area(x, na.rm = TRUE)
##     ar <- na.omit(values(ar))
##     ar. <- length(ar)*median(ar)
##     return(ar.)}

## fa <- function(x, fun = 'median') {
##     v <- area(x, na.rm = TRUE)
##     y <- cellStats(x, stat = sum)
##     if(fun == 'mean')
##         z <- cellStats(v, stat = fun)
##     if(fun == 'median')
##         z <- quantile(v, probs = 0.5,
##                       names = FALSE)
##     ar <- y * z  
##     return(ar)}

f2 <- function(x, a) {
    v <- getValues(x)
    v[v == 0] <- NA
    v[!v%in%a] <- NA
    v[v>1] <- 1
    x <- setValues(x, v)
    return(x)
}

f8 <- function(x, a, filename='', ...) {
    out <- raster(x)
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
        filename <- rasterTmpFile()
    }
    if (filename != '') {
        out <- writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol=nrow(out), nrow=ncol(out))
        todisk <- FALSE
    }
    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)
    if (todisk) {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[v == 0] <- NA
            v[!v%in%a] <- NA
            v[v>1] <- 1
            out <- writeValues(out, v, bs$row[i])
            pbStep(pb, i)
        }
        out <- writeStop(out)
    } else {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[v == 0] <- NA
            v[!v%in%a] <- NA
            v[v>1] <- 1
            cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
            vv[,cols] <- matrix(v, nrow=out@ncols)
            pbStep(pb, i)
        }
        out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
}

f16 <- function(x, a, filename='', ...) {
    out <- raster(x)
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
        filename <- rasterTmpFile()
    }
    if (filename != '') {
        out <- writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol=nrow(out), nrow=ncol(out))
        todisk <- FALSE
    }
    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)
    if (todisk) {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[!v%in%a] <- NA
            out <- writeValues(out, v, bs$row[i])
            pbStep(pb, i)
        }
        out <- writeStop(out)
    } else {
        for (i in 1:bs$n) {
            v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
            v[!v%in%a] <- NA
            cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
            vv[,cols] <- matrix(v, nrow=out@ncols)
            pbStep(pb, i)
        }
        out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
}


fperc <- function(x) {
    v <- getValues(x)
    v[v > 100] <- NA
    x <- setValues(x, v)
    return(x)
}

getlyr <- function(x, nm.){
    nm. <- paste(nm., collapse = '|')
    x[[names(x)[grepl(nm., names(x))]]]}

isWin <- Sys.info()['sysname']%in%'Windows'


loadFromZip <- function(pt., patt = '.tif',
                        mc.cores = detectCores()){
    ## fld <- dir(pt.)
    ## zif <- fld[grepl('.zip', fld)]
    ## pt. <- file.path(pt., zif)
    
    tmp. <- tempdir()
    temp. <- file.path(tmp., 'data')
    fprll <- getOption('fapp')
    if(!getOption('isWin'))
        marg[['mc.cores']] <- mc.cores
    marg. <- c(list(FUN = function(x)
        unzip(x,exdir = tmp., list = TRUE),
        x = pt.),marg)
    lstar. <- do.call(fprll, marg.)
    lstar. <- unlist(lstar.)
    lstar. <- lstar.[grepl('.tif', lstar.)]
    lstar.. <- file.path(tmp.,lstar.)
    if(!all(lstar.%in%dir(tmp.))){
        marg.$'FUN' <- function(x)
            unzip(x,exdir = tmp.)
        lstr.. <- do.call(fprll, marg.)
        lstar.. <- unlist(lstr..)
        lstar.. <- lstar..[grepl('.tif', lstar..)]
    }
    lstar.. <- lstar..[grepl(patt, lstar..)]
    tifimag <- Map(function(x)
        raster(x),
        x = lstar..)
    aa <- names(tifimag)
    ab <- basename(aa)
    ac <- sub('.tif','',ab)
    names(tifimag) <- ac
    return(tifimag)}

lsm_l_tafc <- function(msk){
    msk <- stack(msk)
    cells <- cellStats(msk, sum)
    med <- Reduce('*',res(msk)[1:2])
    area <- cells * med
    area <- area/1E4
    return(area)}

marg <- list(SIMPLIFY = FALSE)

myClamp <- function(x, lw, up)
    raster::clamp(x, lw, up, useValues = FALSE)

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

scaleYear <- function(yr.){
scaleYr <- function(year){
    if(any(year >= 2E3))
    year <- as.vector(scale(year,2E3,1))
    return(year)}
sc <- sapply(yr., function(x) scaleYr(x))
return(sc)}

source2Env <- function(zfe, int.patt){
    zfe <- unlist(zfe)
    if(all(grepl('.zip', zfe))){
        zps <- Map(function(x)
            unzip(x, list = TRUE,exdir = tempdir()), zfe)
        znms <- lapply(zps, function(x)x[1L,1L])
        find <- lapply(znms, function(x)x[grepl(int.patt, x)])
        zps <- Map(function(x,y)
            unzip(x, files = y,exdir = tempdir()), x = zfe, y = find)
        zpu <- unlist(zps, use.names = FALSE)
    } else {
        zpu <- zfe                                             
    }
    tifimag <- Map(function(x)
        raster(x), x = zpu)
    return(tifimag)
}

## ziptoEnv <- function(zfe, int.patt){
## zps <- Map(function(x)
##     unzip(x, list = TRUE,exdir = tempdir()), zfe)
## znms <- lapply(zps, function(x)x[1L,1L])
## find <- lapply(znms, function(x)x[grepl(int.patt, x)])
## zps <- Map(function(x,y)
##     unzip(x, files = y,exdir = tempdir()), x = zfe, y = find)
## zpu <- unlist(zps, use.names = FALSE)
## tifimag <- Map(function(x)
##     raster(x), x = zpu)
## return(tifimag)
## }



## Classes
## setClass('FCMask', contains = 'RasterBrick')
## setClass('FCPolygon', contains = 'RasterBrick')

.onAttach <- function(lib, pkg)
{
  ## unlockBinding("forestChange", asNamespace("forestChange")) 
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet forestChange
        packageStartupMessage(
          "forestChange
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'ecoChange' version ", version) } 

  packageStartupMessage("Type 'citation(\"ecoChange\")' for citing this R package in publications.")
  invisible()
}


.onLoad <- function(libname, pkgname){
op <- options()
op.FC <- list(gfc = "https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.6.html",
              inh = c('SpatialPolygonsDataFrame','Extent','character','NULL'),
              isWin = isWin,
              fapp = 'mcmapply',
              miss = ' Missing layer ',
              trls = c('treecover2000','lossyear'),
              plotCol = list(red = 31, green = 133, blue = 222,
                             maxColorValue = 255)
)

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()
}
