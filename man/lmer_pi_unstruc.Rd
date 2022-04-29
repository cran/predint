% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmer_pi_unstruc.R
\name{lmer_pi_unstruc}
\alias{lmer_pi_unstruc}
\title{Prediction intervals for future observations based on linear random effects models}
\usage{
lmer_pi_unstruc(
  model,
  newdat = NULL,
  m = NULL,
  alternative = "both",
  alpha = 0.05,
  nboot = 10000,
  lambda_min = 0.01,
  lambda_max = 10,
  traceplot = TRUE,
  n_bisec = 30
)
}
\arguments{
\item{model}{a random effects model of class lmerMod}

\item{newdat}{a \code{data.frame} with the same column names as the historical data
on which the model depends}

\item{m}{number of future observations}

\item{alternative}{either "both", "upper" or "lower". \code{alternative} specifies
if a prediction interval or an upper or a lower prediction limit should be computed}

\item{alpha}{defines the level of confidence (1-\code{alpha})}

\item{nboot}{number of bootstraps}

\item{lambda_min}{lower start value for bisection}

\item{lambda_max}{upper start value for bisection}

\item{traceplot}{plot for visualization of the bisection process}

\item{n_bisec}{maximal number of bisection steps}
}
\value{
If \code{newdat} is specified: A \code{data.frame} that contains the future data,
 the historical mean (hist_mean), the calibrated coefficient (quant_calib),
 the prediction standard error (pred_se), the prediction interval (lower and upper)
 and a statement if the prediction interval covers the future observation (cover).

 If \code{m} is specified: A \code{data.frame} that contains the number of future observations (m)
 the historical mean (hist_mean), the calibrated coefficient (quant_calib),
 the prediction standard error (pred_se) and the prediction interval (lower and upper).

 If \code{alternative} is set to "lower": Lower prediction limits are computed instead
 of a prediction interval.

 If \code{alternative} is set to "upper": Upper prediction limits are computed instead
 of a prediction interval.

 If \code{traceplot=TRUE}, a graphical overview about the bisection process is given.
}
\description{
lmer_pi_unstruc calculates a bootstrap calibrated prediction interval for one or more
future observation(s) based on linear random effects models as described in section
3.2.4. of Menssen and Schaarschmidt (2021).
Please note, that the bootstrap calibration used here does not consider the sampling
structure of the future data, since the calibration values are drawn randomly from
bootstrap data sets that have the same structure as the historical data.
}
\details{
This function returns a bootstrap calibrated prediction interval
\deqn{[l,u] = \hat{y} \pm q \sqrt{\hat{var}(\hat{y} - y)}}
with \eqn{\hat{y}} as the predicted future observation,
\eqn{y} as the observed future observations, \eqn{\sqrt{\hat{var}(\hat{y} - y)}}
as the prediction standard error and \eqn{q} as the bootstrap calibrated coefficient that
approximates a quantile of the multivariate t-distribution. \cr
Please note that this function relies on linear random effects models that are
fitted with lmer() from the lme4 package. Random effects have to be specified as
\code{(1|random_effect)}.\cr
}
\examples{
# loading lme4
library(lme4)

# Fitting a random effects model based on c2_dat_1
fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
summary(fit)

# Prediction interval using c2_dat2 as future data
\donttest{lmer_pi_unstruc(model=fit, newdat=c2_dat2, alternative="both", nboot=100)}

# Upper prediction limit for m=3 future observations
\donttest{lmer_pi_unstruc(model=fit, m=3, alternative="upper", nboot=100)}

# Please note that nboot was set to 100 in order to decrease computing time
# of the example. For a valid analysis set nboot=10000.

}
\references{
Menssen and Schaarschmidt (2021): Prediction intervals for all of M future
observations based on linear random effects models. Statistica Neerlandica,
 \doi{10.1111/stan.12260}
}