#' @title myncurve
#'
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#' @param a value that you want to find the area to the left of
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return a graph of the area under the curve and the area under the curve
#' @export
#'
#' @examples myncurve(mu=10,sigma=5,a=6)
myncurve = function(mu, sigma, a){
  x <- NULL
  #plot the normal curve +/- 3 sd from mean
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  #shade the area under the curve up to a
  xcurve=seq(mu-3*sigma, a, length=1000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Blue")
  #Calculate probability as a list output
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob

}
