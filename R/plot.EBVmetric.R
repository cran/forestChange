plot.EBVmetric <- structure(function#EBV-metric plot
###A plot of \code{\link{EBVmetric}} is printed.
(
    x, ##<<\code{\link{tibble}}. Data set of metrics such as that
       ##produced by \code{\link{EBVmetric}}.
    ... ##<<Further arguments not implemented here.
){
    data <- x
    ell <- list(...) 
    if(is.numeric(data$'layer')){
        ggplot(data, aes(x= data$'layer', y = data$'value', color = data$'id')) + 
            geom_line(size = 1, color = "grey20") + 
            facet_wrap(~metric, scales = "free_y") +
            labs(y = "value", x = "layer") +
            theme_bw() +
            theme(axis.text.x = element_text(colour = "grey30", size = 15, angle = 90, hjust = 0.5, vjust = 0.5),
                  axis.text.y = element_text(colour = "grey20", size = 15),
                  text = element_text(size = 16))
    } else {
        ggplot(data, aes(x=data$"layer", y=data$"value")) + 
            geom_segment(aes(x=data$"layer", 
                             xend=data$"layer", 
                             y=0, 
                             yend=data$"value")) +
            geom_point(size=3, color = 'grey20') + 
            facet_wrap(~metric, scales = "free_x") +
            theme_bw() +
            theme(axis.text.x = element_text(colour = "grey30", size = 15, angle = 90, hjust = 0.5, vjust = 0.5),
                  axis.text.y = element_text(colour = "grey20", size = 15),
                  text = element_text(size = 16)) +
            coord_flip()
    }
    ## ggplot(data, aes(layer, value, color = id)) + 
    ##     geom_line(size = 1) + 
    ##     facet_wrap(~metric, scales = "free") +
    ##     labs(y = "value") +
    ##     theme_bw() +
    ##     theme(axis.text.x = element_text(colour = "grey30", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
    ##           axis.text.y = element_text(colour = "grey20", size = 12),
    ##           text = element_text(size = 16))
### \code{plot}.
} , ex=function(){
    ## \donttest{
    ## mpio <- 'Uribia'
    ## msk <- FCMask(mpio, year = 10:17)
    ## met <- EBVmetric(msk, what = 'lsm_l_frac_mn')
    ## plot(met)
    ## }
})
