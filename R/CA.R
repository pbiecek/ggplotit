#' Plot the CA Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.CA} plots objects of the class CA{FactoMineR}
#'
#' @param x An object of class CA
#' @param arrows If TRUE arrows will be ploted
#' @param names List with two vectors - names of rows and columns of contingence table. If NULL then names are derived from CA object. 
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(FactoMineR)
#' obj <- CA(children, row.sup = 15:18, col.sup = 6:8)
#' ggplotit(obj)
#' }
#' @export
#' @import ggplot2

ggplotit.CA <- function(x, arrows=c(FALSE, FALSE), names=NULL, ...) {
  stopifnot(length(arrows) == 2)
  stopifnot(length(names) == 2 | is.null(names))
  
  X <- as.data.frame(x$row$coord[,1:2])
  Y <- as.data.frame(x$col$coord[,1:2])
  if (!is.null(names)) {
    X$Names <- names[[1]]
    Y$Names <- names[[2]]
  } else {
    X$Names <- rownames(x$row$coord)
    Y$Names <- rownames(x$col$coord)
  }
  colnames(X) <- c("x.Dim1","x.Dim2", "x.Names")
  colnames(Y) <- c("y.Dim1","y.Dim2", "y.Names")
  
  # ggplot2
  pl <- ggplot() +
    geom_text(data=X, aes(x.Dim1, x.Dim2, label=x.Names), color="blue", size=3) + 
    geom_text(data=Y, aes(y.Dim1, y.Dim2, label=y.Names), color="red", size=3) + 
    geom_hline(yintercept=0, alpha=0.5) + 
    geom_vline(xintercept=0, alpha=0.5) + 
    theme_bw()
  
  if (arrows[1]) {
    pl <- pl + geom_segment(data=X, aes(x=0, xend=x.Dim1, y=0, yend=x.Dim2, label=x.Names), color="blue",
                            arrow = arrow(angle=15))
  }
  if (arrows[2]) {
    pl <- pl + geom_segment(data=Y, aes(x=0, xend=y.Dim1, y=0, yend=y.Dim2, label=y.Names), color="red",
                            arrow = arrow(angle=15))
  }
  pl
}
