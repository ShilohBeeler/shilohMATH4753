#' @title Binomial simulation over multiple iterations
#'
#' @param iter The number of simulations to run
#' @param n The number of elements
#' @param p The probability of success
#'
#' @return Outputs a barplot and a table with the data from the simulation.
#' @export
#'
#' @examples
#' \dontrun{mybin(100, 10, .7)}
mybin=function(iter=100,n=10, p=0.5){
  # Create a matrix to hold all data, and a vector for successes.
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  # Populate the matrix with samples and populate the successes from the samples.
  for(i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  # Make a table of successes and a barplot for the proportions
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
