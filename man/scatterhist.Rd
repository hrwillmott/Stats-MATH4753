% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterhist.R
\name{scatterhist}
\alias{scatterhist}
\title{scatterhist}
\usage{
scatterhist(x, y, xlab = "", ylab = "")
}
\arguments{
\item{x}{x values}

\item{y}{y values}

\item{xlab}{x axis label}

\item{ylab}{y axis label}
}
\value{
a scatter histogram of y~x
}
\description{
scatterhist
}
\examples{
\dontrun{with(ddt, scatterhist(LENGTH,WEIGHT, xlab="LENGTH"))}
}
