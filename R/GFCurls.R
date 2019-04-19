GFCurls <- structure(function #URLs of GFC data
### This function retrieves \code{URL} of Global Forest Change
### (\code{GFC}) data.
                     ##references<<\href{http://earthenginepartners.appspot.com}{https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html}
(
    lyrs = c('treecover2000','lossyear'), ##<<\code{character}. Name(s)
                                          ##of the layers. Default
                                          ##\code{'treecover2000'},
                                          ##and \code{'lossyear'}
    url = NULL ##<<\code{character}.  Path to the \code{html} file
               ##containing the files. Default \code{NULL} retrieves
               ##\code{URL}s stored in the application programming
               ##interface of \code{GFC}, see \code{References}.

) {
    if(!curl::has_internet())
        return("no internet")
    if(is.null(url)) url <-
    "https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.6.html"
    doc <- xml2::read_html(url)
    nod <- rvest::html_nodes(doc, 'a')
    href <- rvest::html_attr(nod, "href")
    lnks <- href[grepl('.txt', href)]
    lyrs <- paste('\\b', lyrs, '\\b', sep = "")
    lnks <- lnks[grepl(paste(lyrs, collapse = '|'), lnks)]
    ## enco. <- "unknown"
    enco. <- "latin1"
    ## if(Sys.info()['sysname']%in%'Linux')
    ## enco. <- 'UTF-8'
    fchImp <- function(x){
        as.character(read.table(x, encoding = enco., skipNul = TRUE)[,1L])}
    llnks <- Map(function(x)
        fchImp(x), lnks)
    vlnks <- unlist(llnks, use.names = FALSE)
    return(vlnks)
### \code{character} vector.
} , ex=function() {
    ## \donttest{
    ## gainLayers <- GFCurls(lyrs = 'gain')
    ## head(gainLayers)
    ## }
})
