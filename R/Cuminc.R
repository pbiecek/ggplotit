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
  obj <- attr(x, "survfit")
  mat <- obj$cumhaz[dim(obj$cumhaz)[1],,]
  tmp <- data.frame(time = obj$time, t(mat))
  tmp_long <- tidyr::gather(tmp[,-ncol(tmp)], var, val, -time)

  # ggplot2
  pl <- ggplot(tmp_long, aes(time, val, color=var)) +
    geom_point() +
    geom_step()

  pl
}
