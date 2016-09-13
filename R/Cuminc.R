#' Plot the Multi State Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.survfitms} plots objects of the class survfitms{survival}
#' Calculate nonparametric cumulative incidence functions.
#' Competing Risks Model.
#'
#' @param x An object of class survfitms
#' @param ... Other parameters
#' @param conf.int shall the confidence interval be plotted?
#' @param labels shall labels be added?
#' @param breaks coordinates at whcih CI bars shall be added
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(mstate)
#' data(aidssi)
#' ci <- survfit(Surv(time=aidssi$time, status=aidssi$status)~1)
#' ggplotit(ci)
#' }
#' @export
#' @rdname survfitms
#' @import ggplot2

ggplotit.survfitms <- function(obj, conf.int=FALSE, breaks=NULL, labels=NULL, ...) {
  mat <- obj$prev
  matL <- obj$lower
  matU <- obj$upper
  tmp <- data.frame(time = obj$time,
                    mat[,-ncol(mat)])
  tmpL <- data.frame(time = obj$time,
                     matL[,-ncol(matL)])
  tmpU <- data.frame(time = obj$time,
                     matU[,-ncol(matU)])

  # is strata presented?
  if (length(obj$n)>1) {
    tmp$strata <- rep(names(obj$strata), obj$strata)
  } else {
    tmp$strata <- ""
  }

  tmp_long <- tidyr::gather(tmp, event, val, -time, -strata)
  tmp_longL <- tidyr::gather(tmpL, varL, valL, -time)
  tmp_longU <- tidyr::gather(tmpU, varU, valU, -time)
  tmp_long <- cbind(tmp_long, tmp_longL, tmp_longU)

  if (!is.null(labels)) {
    tmp_long$event <- factor(tmp_long$event, labels=labels[-1])
  }

  # ggplot2
  pl <- ggplot(tmp_long, aes(time, val, color=event, group=paste(strata, event))) +
    geom_step(aes(linetype=strata), direction="hv") + ylab("prevalence")
  if (conf.int) {
    pl <- pl + geom_ribbon(aes(time, ymin=valL, ymax=valU, fill=event), alpha=0.5, linetype=0)
  }
  if (!is.null(breaks) & length(breaks)>0) {
    tmp_long$se <- paste(tmp_long$strata, tmp_long$event)
    ind <- unlist(
      lapply(breaks, function(b) {
        lapply(unique(tmp_long$se),
               function(use)
                 min(which(tmp_long$time >= b & tmp_long$se == use)))
      }))
    tmp_long_sel <- tmp_long[ind,]
    pl <- pl + geom_errorbar(data=tmp_long_sel, aes(time, ymin=valL, ymax=valU, group=se, color=event), width=0.3)
  }

  pl
}

#' Plot the Cuminc Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.Cuminc} plots objects of the class Cuminc{mstate}
#' Calculate nonparametric cumulative incidence functions.
#' Competing Risks Model.
#'
#' @param x An object of class Cuminc
#' @param conf.int - shall the confidence interval be plotted?
#' @param labels - shall labels be added?
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(mstate)
#' data(aidssi)
#' ci <- Cuminc(time=aidssi$time, status=aidssi$status)
#' ggplotit(ci)
#' ggplotit(ci, conf.int=TRUE)
#' ggplotit(ci, conf.int=TRUE, labels=c("event-free","AIDS","SI"))
#' }
#' @export
#' @rdname Cuminc
#' @import ggplot2


ggplotit.Cuminc <- function(x, conf.int=FALSE, labels=NULL, ...) {
  obj <- attr(x, "survfit")
  ggplotit.survfitms(obj, conf.int = conf.int, labels = labels, ...)
}

