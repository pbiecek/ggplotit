#' Plot Objects with the ggplot2 Package
#'
#' Generic function.
#'
#' @param x An object.
#' @param ... Other parameters.
#' @export

ggplotit <- function(x, ...) {
  UseMethod("ggplotit")
}