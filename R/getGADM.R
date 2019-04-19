getGADM <- structure(function #Get Geographic Adminitrative Unit
### This function is a wrapper of \code{\link{getData}} used to import
### levels in Geographic Administrative Units (\code{GADM}).
                     ##references<<\href{https://gadm.org/}{https://gadm.org/}
(
    unit.nm = NULL, ##<<\code{character} or \code{NULL}. A name in the
                    ##administrative units (e.g. municipalities), or
                    ##the name of the unit plus its corresponding
                    ##higher-level unit (e.g. department/state). If
                    ##\code{NULL} then a list of unit names
                    ##corresponding to \code{'level'} is printed.
    level = 2, ##<<\code{numeric}. A number between zero and two,
               ##indicating any of the levels of administrative
               ##subdivisions in \code{GADM}: \code{0=country},
               ##\code{1=first level of subdivision}, and
               ##\code{2=second level of subdivision}).
    country = 'COL' ##<<\code{character}. \code{ISO} code specifying a
                    ##country. Default \code{'COL'}
) {
    adm <- getData('GADM',
                   country=country,
                   level=level)
    if(level%in%0)
        return(adm)
    lv.col <- paste('NAME',level, sep ='_')
    ds <- data.frame(adm)[,lv.col]
    if(is.null(unit.nm))
        return(ds)
    chm <- pmatch(unit.nm[1], ds)
    unit.nm[1] <- ds[chm] 
    adm <- subset(adm, get(lv.col)%in%unit.nm[1])
    if(length(adm) == 0)
        stop("'unit.nm' not found, change 'level'/'country'")
    if(level == 2 & length(adm) > 1)
        if(length(unit.nm) == 1)
            stop("Ambiguous 'unit.nm',
a higher-level subdivision is required: unit.nm = c('unit', 'h.l.unit')")
    if(length(unit.nm) > 1){
        ds <- data.frame(adm)[,'NAME_1']
        chm <- pmatch(unit.nm[2], ds)
        unit.nm[2] <- ds[chm] 
        adm <- subset(adm, get('NAME_1')%in%unit.nm[2])}
    return(adm)
### List of rasters.
} , ex=function() {
## Printing municipalities of Colombia:    
## \donttest{
    ## muni <- getGADM()
    ## head(muni)
## }

})
