#' @title A function that generates a 95 percent confidence interval for the population mean
#'
#' @param x The sample to generate the confidence interval from.
#'
#' @return The 95 percent confidence interval values.
#' @export
#'
#' @examples
#' \dontrun{set.seed(23);x = rnorm(30,mean=10,sd=12);myci(x)}
myci <- function(x) {
  c(mean(x)-qt(0.975,length(x)-1)*sd(x)/sqrt(length(x)),mean(x)+qt(0.975,length(x)-1)*sd(x)/sqrt(length(x)))
}
