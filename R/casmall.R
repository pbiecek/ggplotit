#' Plot the ca Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.ca} plots objects of the class ca{ca}
#'
#' @param x An object of class ca
#' @param arrows If TRUE arrows will be ploted
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(ca)
#' obj <- ca(haireye)
#' ggplotit(obj)
#' }
#' @export
#' @import ggplot2

ggplotit.ca <- function(x, arrows=c(FALSE, FALSE), ...) {
  stopifnot(length(arrows) == 2)
  
  X <- as.data.frame(x$rowcoord[,1:2])
  X$Names <- rownames(x$rowcoord)
  Y <- as.data.frame(x$colcoord[,1:2])
  Y$Names <- rownames(x$colcoord)
  colnames(X) <- c("x.Dim1","x.Dim2", "x.Names")
  colnames(Y) <- c("y.Dim1","y.Dim2", "y.Names")
  
  # ggplot2
  pl <- ggplot() +
    geom_text(data=X, aes(x.Dim1, x.Dim2, label=x.Names), color="blue", size=3) + 
    geom_text(data=Y, aes(y.Dim1, y.Dim2, label=y.Names), color="red", size=3) + 
    geom_hline(xintercept=0, alpha=0.5) + 
    geom_vline(yintercept=0, alpha=0.5) + 
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
