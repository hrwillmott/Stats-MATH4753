#' @title ntickets - a function that determines the number of tickets an airline should sell
#'
#' @param N number of available seats on the flight
#' @param gamma probability plane will truly be overbooked
#' @param p probability any given passenger will show up
#' @importFrom graphics points
#' @importFrom stats optimize pbinom
#'
#' @return plots of binomial distribution and normal approximation, and the number of tickets to be sold in each case
#' @export
#'
#' @examples ntickets(400,0.02,0.95)
ntickets = function(N,gamma,p){
  #Creating a plot of the Discrete Objective vs. n

  #x limits: Sequence of natural numbers because the binomial is a discrete distribution. Started at N because overbooking only becomes a problem when we sell more than N tickets, and ended at N+50 because it should be a sufficiently large range in most cases.
  x = seq(N,N+50,1)
  #R code for plotting the function
  plot(x, 1-gamma-pbinom(N,x,p),pch=20, col="blue", ylab="Objective", xlab="n", main = paste0("Discrete Objective vs. n with N=",N," gamma=",gamma," p=",p), type="o", cex=(.8))

  #finding nd--found min of abs(objective function), translate the index to value of n, and color the point on our plot red.
  xmin <- which.min(abs(1-gamma-pbinom(N,x,p)))
  nd <- x[xmin]
  points(nd,1-gamma-pbinom(N,nd,p),pch=20,col="red")

  #plot of normal approximation
  curve(1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p))),xlim=c(N,N+50),xlab="n", ylab="Objective", main=paste0("Continuous Objective vs. n with N=",N," gamma=",gamma," p=",p))

  #finding nc and plotting the point on the continuous objective plot
  nc= NULL
  f = function(x){
    gamma = gamma
    N = N
    p = p
    abs(1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p))))
  }
  op <- optimize(f,interval=c(N,N+50))
  nc <- op$minimum
  points(nc,1-gamma-pnorm(N+.05,nc*p,sqrt(nc*p*(1-p))),pch=20,col="red")

  #List of N, gamma, p, nd, nc
  print(paste0("N = ", N), quote = FALSE)
  print(paste0("gamma = ", gamma), quote = FALSE)
  print(paste0("p = ", p), quote = FALSE)
  print(paste0("nd = ", nd), quote = FALSE)
  print(paste0("nc = ", nc), quote = FALSE)
}
