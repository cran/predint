% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmer_pi_futvec.R
\name{lmer_pi_futvec}
\alias{lmer_pi_futvec}
\title{Prediction intervals for future observations based on linear random effects models}
\usage{
lmer_pi_futvec(
  model,
  futvec,
  newdat = NULL,
  alternative = "both",
  alpha = 0.05,
  nboot = 10000,
  delta_min = 0.01,
  delta_max = 10,
  tolerance = 0.001,
  traceplot = TRUE,
  n_bisec = 30
)
}
\arguments{
\item{model}{a random effects model of class lmerMod}

\item{futvec}{an integer vector that defines the structure of the future data based on the
row numbers of the historical data. If \code{length(futvec)} is one, a PI
for one future observation is computed}

\item{newdat}{a \code{data.frame} with the same column names as the historical data
on which  \code{model} depends}

\item{alternative}{either "both", "upper" or "lower". \code{alternative} specifies
if a prediction interval or an upper or a lower prediction limit should be computed}

\item{alpha}{defines the level of confidence (1-\code{alpha})}

\item{nboot}{number of bootstraps}

\item{delta_min}{lower start value for bisection}

\item{delta_max}{upper start value for bisection}

\item{tolerance}{tolerance for the coverage probability in the bisection}

\item{traceplot}{plot for visualization of the bisection process}

\item{n_bisec}{maximal number of bisection steps}
}
\value{
If \code{newdat} is specified: A \code{data.frame} that contains the future data,
 the historical mean (hist_mean), the calibrated coefficient (quant_calib),
 the prediction standard error (pred_se), the prediction interval (lower and upper)
 and a statement if the prediction interval covers the future observations (cover).

 If only \code{futvec} is specified:
 A \code{data.frame} that contains the number of future observations (m)
 the historical mean (hist_mean), the calibrated coefficient (quant_calib),
 the prediction standard error (pred_se) and the prediction interval (lower and upper).
 If \code{futvec} is set to 1, the PI is calculated for one future observation.

 If \code{alternative} is set to "lower": Lower prediction limits are computed instead
 of a prediction interval.

 If \code{alternative} is set to "upper": Upper prediction limits are computed instead
 of a prediction interval.

 If traceplot=TRUE, a graphical overview about the bisection process is given.
}
\description{
lmer_pi_futvec calculates a bootstrap calibrated prediction interval for one or more
future observation(s) based on linear random effects models. With this approach,
the sampling structure of the future data is taken into account (see below).
}
\details{
This function returns a bootstrap calibrated prediction interval
\deqn{[l,u] = \hat{y} \pm q \sqrt{\hat{var}(\hat{y} - y)}}
with \eqn{\hat{y}} as the predicted future observation,
\eqn{y} as the observed future observations, \eqn{\sqrt{\hat{var}(\hat{y} - y)}}
as the prediction standard error and \eqn{q} as the bootstrap calibrated coefficient that
approximates a quantile of the multivariate t-distribution. \cr
Please note that this function relies on linear random effects models that are
fitted with \code{lmer()} from the lme4 package. Random effects have to be specified as
\code{(1|random_effect)}.

Be aware that the sampling structure of the historical data must contain the structure of the
future data. This means that the observations per random factor must be less or
equal in the future data compared to the historical data.

This function is an implementation of the PI given in Menssen and Schaarschmidt 2021 section 3.2.4
except that the bootstrap calibration values are drawn from bootstrap samples that
mimic the future data.
}
\examples{

# loading lme4
library(lme4)

# Fitting a random effects model based on c2_dat1
fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
summary(fit)

#----------------------------------------------------------------------------

### Prediction interval using c2_dat3 as future data
# without printing c2_dat3 in the output

# Row numbers of the historical data c2_dat1 that define the structure of
# the future data c2_dat3
futvec <- c(1, 2, 4, 5, 10, 11, 13, 14)

# Calculating the PI
\donttest{lmer_pi_futvec(model=fit, futvec=futvec, alternative="both", nboot=100)}

#----------------------------------------------------------------------------

### Calculating the PI with c2_dat3 printed in the output
\donttest{lmer_pi_futvec(model=fit, futvec=futvec, newdat=c2_dat3, alternative="both", nboot=100)}

#----------------------------------------------------------------------------

### Upper prediction limit for m=1 future observation
\donttest{lmer_pi_futvec(model=fit, futvec=1, alternative="upper", nboot=100)}

#----------------------------------------------------------------------------

# Please note that nboot was set to 100 in order to decrease computing time
# of the example. For a valid analysis set nboot=10000.

}
\references{
Menssen, M., Schaarschmidt, F.: Prediction intervals for all of M
future observations based on linear random effects models. Statistica Neerlandica.
\doi{10.1111/stan.12260}
}
