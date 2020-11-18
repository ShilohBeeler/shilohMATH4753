#' @title A function which outputs the t statistic for given sample data.
#'
#' @param z The sample data to return the t statistic for.
#'
#' @return The t-statistic for the sample data.
#' @export
#'
#' @examples
#' \dontrun{mytstat(c(1,2,3,4,5))}
mytstat<-function(z){ # The value of t when the NULL is assumed true (xbar-muo)/z/sqrt(n)
  sqrt(length(z))*(mean(z)-mu0)/sd(z)
}
