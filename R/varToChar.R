#' A Cat Function
#'
#' This function allows you chenage variable names to a string
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords Glib
#' @export
#' @examples
#' varToChar()

varToChar <- function(v1) {
  deparse(substitute(v1))
}


