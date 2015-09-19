#' Plot the table Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.table} plots objects of the class table{base}
#'
#' @param x An object of class table
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(FactoMineR)
#' ggplotit(as.table(children))
#' }
#' @export
#' @import ggplot2

ggplotit.table <- function(x, ...) {
  stopifnot(length(dim(x)) == 2)

  df <- data.frame(x)
  dfn <- colnames(df)
  ggplot(df, aes_string(x=dfn[1], fill=dfn[2], y=dfn[3])) +
    geom_bar(stat="identity", position="fill")
}
