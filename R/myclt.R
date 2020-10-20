#' @title Central Limit Theorem function for the sum of a uniform distribution.
#'
#' @param n The sample size.
#' @param iter How many samples (iterations) to take.
#' @param a The lower limit of the uniform distribution.
#' @param b The upper limit of the uniform distribution.
#'
#' @return Outputs a histogram of the sums of each sample taken, and a matrix containing the sums.
#' @export
#'
#' @examples
#' \dontrun{myclt(10, 10000)}
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
