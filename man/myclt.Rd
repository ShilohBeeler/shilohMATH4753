% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myclt.R
\name{myclt}
\alias{myclt}
\title{Central Limit Theorem function for the sum of a uniform distribution.}
\usage{
myclt(n, iter, a = 0, b = 5)
}
\arguments{
\item{n}{The sample size.}

\item{iter}{How many samples (iterations) to take.}

\item{a}{The lower limit of the uniform distribution.}

\item{b}{The upper limit of the uniform distribution.}
}
\value{
Outputs a histogram of the sums of each sample taken, and a matrix containing the sums.
}
\description{
Central Limit Theorem function for the sum of a uniform distribution.
}
\examples{
\dontrun{myclt(10, 10000)}
}
