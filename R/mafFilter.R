#' A VCF read (by variantannotator) function to filter MAF
#'
#' This function filter var over a thresgold returns a indexlist of variants under cutoff
#' if LOW is set to TRUE and a indexlist if LOW is FALSE
#' @param a vector of MAFs,maf cutoff value, LOW=TRUE
#' @keywords Glib
#' @export
#' @examples
#' mafFilter()

mafFilter <- function(Y,maf,LOW=TRUE){
  res <- sapply(1:length(Y),function(x) {

    d <- unlist(Y[[x]])
    d[is.na(d)] <- 0

    if (length(d) > 1) {
      d <- max(d)
    }

    if (LOW == TRUE) {
      return(sum(d <= maf) >= 1)
    }

    if (LOW == FALSE) {
      return(sum(d > maf) >= 1)
    }
  })
  return(which(res))
}
