#' @title Normal curve from negative infinity to a.
#'
#' @param mu The mean of the normal distribution.
#' @param sigma The standard deviation of the normal distribution.
#' @param a The x value that you want to draw the curve up to.
#'
#' @return Outputs a graph with the normal curve shaded from negative infinity to a and the area in that interval.
#' @export
#'
#' @examples
#' \dontrun{myncurve(0, 1, .5)}
myncurve <- function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma))
  xcurve = seq(-99999, a, length.out = 1000000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(-99999, xcurve, a),c(0,ycurve,0),col="Red")
  area = pnorm(a, mean=mu, sd=sigma)
  area = round(area, 4)
  paste0("Area=", area)
}
