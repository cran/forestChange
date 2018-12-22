GFCurls <- structure(function #URLs of GFC
### This function can find lists of \code{URL} necessary to download Global
### Forest Change data (\code{GFC}).
                     ##details<<The function is implemented by
                     ##\code{\link{FCPolygon}}.
                     ##references<<\href{http://earthenginepartners.appspot.com}{http://earthenginepartners.appspot.com/science-2013-global-forest}
(
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Portion
                                          ##of the \code{URL}s matching
                                          ##names of \code{GFC}
                                          ##layers. Default
                                          ##\code{'treecover2000'},
                                          ##and \code{'lossyear'}
    gglapi = NULL, ##<<\code{character}.  Portion of an \code{URL}
                   ##which is common to the set of \code{URL}s to be
                   ##retrieved. If \code{NULL} then an application
                   ##programming interface of google is used, see
                   ##\code{References}.
        ext = '.txt' ##<<\code{logical}. Extension of the file
                     ##containing the links. Default \code{'txt'}

    
) {
    if(is.null(gglapi))
        gglapi <- 'https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5'
    if(is.null(lyrs))
        lyrs <- c('treecover2000','gain','lossyear','datamask','first','last')
    lyrs. <- paste(lyrs, ext, sep ='')
    lnks <- paste(gglapi, lyrs., sep = '/')
    llnks <- lapply(lnks, read.table)
    vlnks <- as.character(unlist(do.call('rbind',llnks)))
    attributes(vlnks) <- c(attributes(vlnks),
                           list(lyrs = lyrs))
    return(vlnks)
### \code{character} vector.
} , ex=function() {
    gainLayers <- GFCurls(lyrs = 'gain')
    head(gainLayers)    
})
