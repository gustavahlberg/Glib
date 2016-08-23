#' QQplot of pvalues
#'
#' This function extracts
#' @param  pvector, a vector of pvalues. main default NULL
#' @keywords Glib
#' @export
#' @examples
#' ggdQqplot()

ggdQqplot = function(pvector, main=NULL, ...) {
  o = -log10(sort(pvector,decreasing=F))
  e = -log10( 1:length(o)/length(o) )
  plot(e,o,pch=19,cex=1, main=main, ...,
       xlab=expression(Expected~~-log[10](italic(p))),
       ylab=expression(Observed~~-log[10](italic(p))),
       xlim=c(0,max(e)), ylim=c(0,max(o)))
  lines(e,e,col="red")
}
