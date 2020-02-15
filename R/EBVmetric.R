EBVmetric <- structure(function #EBV metric
### This function formats \code{GFC} and computes structural metrics
### of Essential Biodiversity Variables (\code{EBV} metrics):
### forest-cover areas and landscape metrics, in predefined polygons
### or GADM.
                      ##details<< Coordinate system of the spatial
                      ##units should be \code{ UTM}. Metrics other
                      ##than \code{'lsm_l_tafc'} are calculated
                      ##implementing \code{\link{calculate_lsm}}.
                      ##references<< O'Connor, B., Secades, C.,
                      ##Penner, J., Sonnenschein, R., Skidmore, A.,
                      ##Burgess, N. D., & Hutton, J. M. (2015). Earth
                      ##observation as a tool for tracking progress
                      ##towards the Aichi Biodiversity Targets. Remote
                      ##sensing in ecology and conservation, 1(1),
                      ##19-28.
(
    pol,  ##<<\code{RasterStack}.Stack such as these produced by
          ##\code{\link{FCMask}}.
    what = 'lsm_l_tafc', ##<<\code{character}. The metrics. These
                         ##include \code{'lsm_l_tafc'} to compute
                         ##total forest-cover areas (ha) and other
                         ##landscape metrics in
                         ##\code{\link{calculate_lsm}}. Default
                         ##(\code{'lsm_l_tafc'}).
    mc.cores = detectCores(), ##<<\code{numeric}. The number of cores,
                              ##see \code{\link{mclapply}}.
    ... ##<< additional arguments in \code{\link{calculate_lsm}}.

) {

    lsm_l_tafc <- function(msk){
        msk <- stack(msk)
        cells <- cellStats(msk, sum)
        med <- Reduce('*',res(msk)[1:2])
        area <- cells * med
        area <- area/1E4
        return(area)}

    ftibb <- function(msk, tyr){
        tyr <- as.numeric(
            sub("\\D+","", names(msk)))
        if(all(is.na(tyr)))
            tyr <- names(msk)
        levs <- 'landscape' 
        names(levs) <- c('l')
        mtrs <- c('FC_area'); names(mtrs) <- 'tafc' 
        dim <- nlayers(msk)
        tbb <- tibble(layer = tyr,
                      level = rep(levs, dim),
                      class = rep(NA, dim),
                      id = rep(NA, dim),
                      metric = rep(mtrs, dim),
                      value = lsm_l_tafc(msk))
        return(tbb)
    }

    inw <- grepl('lsm_l_tafc', what)
    yswhat <- what[inw]  
    ntwhat <- what[!inw]
    tyr <- as.numeric(
        sub("\\D+","", names(pol)))
    tyr. <- tyr
    if(all(is.na(tyr)))
        tyr <- names(pol)
        are. <- tibble()
    if(!length(yswhat) == 0)
        are. <- ftibb(pol, tyr = tyr)
        are.. <- tibble()
        if(!length(ntwhat) == 0){
            are.. <- calculate_lsm(pol,
                                   what = what, ...)
            years <- pull(unique(are..['layer']))
            names(years) <- tyr
            yr.. <- pull(are..['layer'])
            are..['layer'] <- names(years)[yr..]
            if(!all(is.na(tyr.)))
                are..['layer'] <- as.numeric(names(years)[yr..])
        }
        mets <- rbind(are., are..)
        class(mets) <- append('EBVmetric',class(mets))
        return(mets)
### \code{tibble}.
} , ex=function() {
    ## \donttest{
    ## mpio <- 'Uribia'
    ## msk <- FCMask(mpio, year = 10:17)
    ## met <- EBVmetric(msk, what = 'lsm_l_frac_mn')
    ## plot(met)
    ## }
})
