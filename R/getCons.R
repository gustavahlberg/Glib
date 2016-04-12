#' A VCF read Function
#'
#' This function extracts
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords Glib
#' @export
#' @examples
#' varToChar()

getCons <- function(Y,con) {
  inf <- info(Y)$EFF
  res <- sapply(1:length(inf), function(i) {
    res <- grepl(con,inf[[i]])
    sum(res) >= 1
  })
  which(res)
}
