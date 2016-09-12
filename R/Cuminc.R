#' Plot the Cuminc Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.Cuminc} plots objects of the class Cuminc{mstate}
#' Calculate nonparametric cumulative incidence functions.
#'
#' @param x An object of class Cuminc
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(mstate)
#' data(aidssi)
#' ci <- Cuminc(time=aidssi$time, status=aidssi$status)
#' ggplotit(ci)
#' }
#' @export
#' @import ggplot2

ggplotit.Cuminc <- function(x, ...) {

  # ggplot2
  pl <- ggplot()

  pl
}
