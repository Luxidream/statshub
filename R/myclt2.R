#' myclt2
#'
#' Samples from a uniform distribution.
#'
#'
#' @param n number to sample
#' @param iter iterations
#' @param a parameter for uniform distribution
#' @param b parameter for uniform distribution
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics hist
#' @importFrom stats runif
#' @importFrom stats dnorm
#'
#' @return None.
#'
#' @examples myclt2(n=10, iter=10000)
#'
#'
#' @export


myclt2=function(n,iter,a=0,b=5){
  x <- NULL
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

#── R CMD check results ────────────────────────────────────────────────────── luxipackage 0.1.0 ────
#Duration: 15.2s
#0 errors ✔ | 0 warnings ✔ | 0 notes ✔
#R CMD check succeeded





