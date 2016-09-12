#' Plot the Cuminc Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.Cuminc} plots objects of the class Cuminc{mstate}
#' Calculate nonparametric cumulative incidence functions.
#' Competing Risks Model.
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
#' ggplotit(ci, conf.int=TRUE)
#' ggplotit(ci, conf.int=TRUE, labels=c("event-free","AIDS","SI))
#' }
#' @export
#' @import ggplot2

ggplotit.Cuminc <- function(x, conf.int=FALSE, labels=NULL, ...) {
  obj <- attr(x, "survfit")
  mat <- obj$prev
  matL <- obj$lower
  matU <- obj$upper
  tmp <- data.frame(time = obj$time,
                    mat[,-ncol(mat)])
  tmpL <- data.frame(time = obj$time,
                    matL[,-ncol(matL)])
  tmpU <- data.frame(time = obj$time,
                    matU[,-ncol(matU)])
  tmp_long <- tidyr::gather(tmp, event, val, -time)
  tmp_longL <- tidyr::gather(tmpL, varL, valL, -time)
  tmp_longU <- tidyr::gather(tmpU, varU, valU, -time)
  tmp_long <- cbind(tmp_long, tmp_longL, tmp_longU)

  if (!is.null(labels)) {
    tmp_long$event <- factor(tmp_long$event, labels=labels[-1])
  }

  # is strata presented?
  if (length(obj$n)>1) {
    tmp_long$strata <- rep(names(obj$strata), each=obj$n)
  } else {
    tmp_long$strata <- ""
  }


  # ggplot2
  pl <- ggplot(tmp_long, aes(time, val, color=event, group=strata)) +
    geom_step() + ylab("prevalence")
  if (conf.int) {
    pl <- pl + geom_ribbon(aes(time, ymin=valL, ymax=valU, fill=event), alpha=0.5, linetype=0)
  }

  pl
}
