HansenUrltoExtent <- structure(function #Extents in GFC links
### This function can extract extents of Global Forest Change data
### (\code{GFC}) using corresponding \code{URL}s.
                     ##details<<The function is implemented by
                     ##\code{\link{FCPolygon}}.
                     ##references<<\href{http://earthenginepartners.appspot.com}{http://earthenginepartners.appspot.com/science-2013-global-forest}
(
    x, ##<<\code{character}. \code{URL} to the \code{GFC}, see
       ##\code{References}.
    path. = "[[:digit:]]{1,3}[N|S|E|W]" ##<<\code{character}. Pattern
                                        ##in the \code{URL} to extract
                                        ##the extent. Default extracts
                                        ##the 3 digits closer to any
                                        ##of the letters \code{N},
                                        ##\code{S}, \code{E}, or
                                        ##\code{W}.
) {

    findCoord <- function(x, path.){
        xfr <- gregexpr(path., x, perl = TRUE)
        xto <- lapply(xfr,function(x)
        (x - 1) + attr(x, 'match.length'))
        ndf <- data.frame(xfr,xto)
        chrc <- apply(ndf,1, function(y)
            substr(x, y[1],y[2]))
        return(chrc)
    }
    of <- findCoord(x, path.)
    tchr <- '[[:digit:]]{1,3}'
    tchr2 <- '[[:upper:]]'
    nus <- sapply(of,function(x,p)
        findCoord(x, tchr))
    chr <- sapply(of,function(x,p)
        findCoord(x, tchr2))
    nu <- as.numeric(nus)
    names(nu) <- chr

    getQuadrants <- function(nu, val, quad){
        nv <- c()
        for(i in 1:length(nu)){
            if(names(nu[i])%in%quad){
                nv[i] <- - val
            }else{
                nv[i] <- val
            }
        }
        return(nv)
    }
    toex1 <- nu * getQuadrants(nu, 1, c('W','S'))
    names(toex1) <- gsub('N|S','y', names(toex1))
    names(toex1) <- gsub('W|E','x', names(toex1))
    toex <- toex1 + getQuadrants(nu, 10, c('N','S'))
    dex <- data.frame(toex1, toex)
    odex <- dex[order(row.names(dex)),]
    ex <- c(apply(odex, 1, function(x)c(min(x), max(x))))
    ext <- extent(ex)
return(ext)
### extent.
} , ex=function() {
urtt <- 'https://storage.googleapis.com/earthenginepartners-hansen/
GFC-2017-v1.5/Hansen_GFC-2017-v1.5_treecover2000_10N_010E.tif'
HansenUrltoExtent(urtt)
    
})
