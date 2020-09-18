#' @title Dot Plot with LM abline
#'
#' @param x The x-axis variable
#' @param data The data frame used for the dotplot and linear model.
#' @param y The y-axis variable
#'
#' @return Outputs a dotplot with the linear model drawn onto it.
#' @export
#'
#' @examples
#' \dontrun{spruce.df<-read.csv("Spruce.df");dotplotwithlm(inputdata=spruce.df, x=BHDiameter, y=Height)}
dotplotwithlm <- function(data, x, y) {
  my.lm = lm(y~x, data=data)
  with(data, plot(y~x,bg="Blue",pch=21,cex=1.1, ylim=c(0,1.1*max(y)),xlim=c(0,1.1*max(x))))
  abline(my.lm)
}
