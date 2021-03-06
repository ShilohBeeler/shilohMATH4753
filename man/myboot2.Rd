% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{A bootstrapping function which finds confidence intervals for population parameters given sample statistics.}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{The number of iterations to run.}

\item{x}{The sample to draw from.}

\item{fun}{The function for the statistic/parameter.}

\item{alpha}{Used in (1-alpha)100 to determine the strength of the confidence interval.}

\item{cx}{Character expansion factor for text.}

\item{...}{Further arguments and graphical parameters passed to hist().}
}
\value{
A graph of the sampled values, the point estimate, and the confidence interval, alongside a returned list of those values.
}
\description{
A bootstrapping function which finds confidence intervals for population parameters given sample statistics.
}
\examples{
\dontrun{sam=rnorm(20); myboot2(x=sam)}
}
