#' Plot the CA objects with ggplot2
#'
#' Function \code{ggplotit.CA} plots objects of the class CA
#'
#' @param x An object of class CA
#' @param arrows If TRUE arrows will be ploted
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(FactoMineR)
#' obj <- CA(children)
#' ggplotit(obj)
#' }
#' @import ggplot2

ggplotit.CA <- function(x, arrows=c(T,T), ...) {
  stopifnot(length(arrows) == 2)
  
  X <- as.data.frame(x$row$coord[,1:2])
  X$Names <- rownames(x$row$coord)
  Y <- as.data.frame(x$col$coord[,1:2])
  Y$Names <- colnames(x$col$coord)
  colnames(X) <- c("x.Dim1","x.Dim2", "x.Names")
  colnames(Y) <- c("y.Dim1","y.Dim2", "y.Names")
  
  # ggplot2
  pl <- ggplot() +
    geom_text(X, aes(x.Dim1, x.Dim2, label=x.Names), size=3) + 
    geom_text(data=Y, aes(y.Dim1, y.Dim2, label=y.Names), color="red", size=3) + 
    geom_hline(xintercept=0, alpha=0.3) + 
    geom_vline(yintercept=0, alpha=0.3) + 
    theme_bw()
  
  if (arrows[1]) {
    pl <- pl + geom_segment(data=X, aes(x=0, xend=x.Dim1, y=0, yend=x.Dim2, label=x.Names), color="red",
                            arrow = arrow(angle=15))
  }
  if (arrows[2]) {
    pl <- pl + geom_segment(data=Y, aes(x=0, xend=y.Dim1, y=0, yend=y.Dim2, label=y.Names), color="red",
                            arrow = arrow(angle=15))
  }
  pl
}
