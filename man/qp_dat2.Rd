% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{qp_dat2}
\alias{qp_dat2}
\title{Quasi-Poisson data (example 2)}
\format{
An integer vector with three entries containing quasi-Poisson data
}
\usage{
qp_dat2
}
\description{
This data set contains sampled quasi-poisson data for 3 clusters.
The data set was sampled with \code{rqpois(n=3, lambda=50, phi=3)}.
}
\examples{
# Prediction interval using qp_dat2 as future data
quasi_pois_pi(histdat=data.frame(qp_dat1), newdat=data.frame(qp_dat2), nboot=100)

# Please note that nboot was set to 100 in order to decrease computing time
# of the example. For a valid analysis set nboot=10000.

}
\keyword{datasets}
