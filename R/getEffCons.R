#'
#' This function runs through a list of snpEFF annotations and returns a index vector
#' of variants with the searched impact (LOW/HIGH/MODERATE/MODERATE)
#' if LOW is set to TRUE and a indexlist if LOW is FALSE
#' @param Y a list of snpEFF annotations, con a string vector
#' @keywords Glib
#' @export
#' @examples getEffCons(info(vcf)$EFF, cons)
#' getEffCons()

getEffCons <- function(Y,con) {

  res <- sapply(1:length(Y), function(i) {


    eff=Y[[i]]

    if(!identical(Y[[i]], character(0))) {
      r <- sapply(1:length(eff), function(j){

        tmp <- unlist(strsplit(eff[j],'\\|'))[1]
        effect <- unlist(strsplit(tmp,'\\('))[2]
        sum(effect == con)

      })
    } else{

      r <-  0

    }

    sum(r) > 0
  })

  which(res)

}
