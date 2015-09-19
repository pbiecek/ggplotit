#' Plot the table Objects with the ggplot2 Package
#'
#' Function \code{ggplotit.table} plots objects of the class table{base}
#'
#' @param x An object of class table
#' @param type One of seq (sequential), div (diverging) or qual (qualitative)
#' @param ... Other parameters
#' @return An ggplot2 plot
#' @examples
#' \dontrun{
#' library(FactoMineR)
#' ggplotit(as.table(children))
#' }
#' @export
#' @import scales
#' @import ggplot2
#' @import RColorBrewer

ggplotit.table <- function(x, type = "seq", ...) {
  stopifnot(length(dim(x)) == 2)

  df <- data.frame(x)
  dfn <- colnames(df)
  ggplot(df, aes_string(x=dfn[1], fill=dfn[2], y=dfn[3])) +
    geom_bar(stat="identity", position="fill") +
    theme_bw() + scale_y_continuous(label=percent) +
    scale_fill_brewer(type=type)
}
