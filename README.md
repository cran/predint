
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predint

<!-- badges: start -->
<!-- badges: end -->

In many pharmaceutical and biomedical applications such as assay
validation, assessment of historical control data or the detection of
anti-drug antibodies, prediction intervals are of use. The package
predint provides functions to calculate bootstrap calibrated prediction
intervals for one or more future observations based on overdispersed
binomial data, overdispersed poisson data, as well as data that is
modeled by linear random effects models fitted with lme4::lmer(). The
main functions are:

-   `beta_bin_pi()` for beta-binomial data (overdispersion differs
    between clusters)

-   `quasi_bin_pi()` for quasi-binomial data (constant overdispersion)

-   `quasi_pois_pi()` for quasi-poisson data (constant overdispersion)

-   `lmer_pi()` for data that is modeled by a linear random effects
    model (deprecated)

-   `lmer_pi_unstruc()` for data that is modeled by a linear random
    effects model. For m = 1: The function follows the same methodology
    as `lmer_pi_futvec()` and `lmer_pi_futmat()`. For m \> 1: This
    function treats the future data to be a random sample from the
    historical experimental design. This may happen if the future trial
    was planned to follow the same design as the historical trial, but
    some observations are randomly missing in the future data.

-   `lmer_pi_futvec()` for data that is modeled by a linear random
    effects model. For m = 1: The function follows the same methodology
    as `lmer_pi_futunstruc()` and `lmer_pi_futmat()`. For m \> 1: This
    function takes care of the experimental design of the future data.
    Anyhow, it is mandatory that the design of the future data is part
    of the historical data.

-   `lmer_pi_futmat()` for data that is modeled by a linear random
    effects model. For m = 1: The function follows the same methodology
    as `lmer_pi_futunstruc()` and `lmer_pi_futvec()`. For m \> 1: This
    function takes care of the experimental design of the future data,
    which can be provided directly to the function. Alternatively the
    random effects design matrices can be provided.

For all of these functions, it is assumed that the historical, as well
as the actual (or future) data descend from the same data generating
process.

## Installation

You can install the released version of predint from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("predint")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MaxMenssen/predint")
```

## Example

The following examples are based on the scenario described in Menssen
and Schaarschmidt 2019: Based on historical control data for the
mortality of male B6C3F1-mice obtained in long term studies at the
National Toxicology Program (NTP 2017), prediction intervals (PI) can be
computed in order to validate the observed mortality of actual (or
future) control groups.

On the one hand, a PI for one future observation can be computed in
order to validate the outcome of one actual (or future) untreated
control group that is compared with several groups treated with the
compound of interest.

On the other hand, in some cases it might be useful to validate the
outcome of several control groups obtained from different trials
simultaneously.

Similarly to Menssen and Schaarschmidt 2019, it is assumed that the data
is overdispersed binomial. Hence, we will use the `quasi_bin_pi()`
function in the following two examples.

### Evaluation of one future control group

In this scenario, it is of interest to validate the control group of an
actual (or future) study that is comprised of 30 mice instead of 50 mice
as in the historical data. For this purpose a single prediction interval
for one future observation is computed.

``` r
# load predint
library(predint)

# Data set 
# see Table 1 of the supplementary material of Menssen and Schaarschmidt 2019
dat_real <- data.frame("dead"=c(15, 10, 12, 12, 13, 11, 19, 11, 14, 21),
                       "alive"=c(35, 40, 38, 38, 37, 39, 31, 39, 36, 29))

# PI for one future control group comprised of 30 mice
pi_m1 <- quasi_bin_pi(histdat=dat_real, 
                      newsize=30,
                      traceplot = FALSE, 
                      alpha=0.05)
pi_m1
#>   total hist_prob quant_calib pred_se    lower    upper
#> 1    30     0.276    1.024609     5.6 2.542187 14.01781
```

The historical binomial probability of success (historical mortality
rate) is 0.276, the bootstrap calibrated coefficient is 1.02461 and the
standard error of the prediction is 5.6. The lower limit of the
bootstrap calibrated asymptotic prediction interval is 2.54219 and its
upper limit is given by 14.01781.

If the mortality is lower than 2.54219 it can be treated as unusual low.
Consequently, mean comparisons between the control and the treatment
groups might result in too many differences that are considered as
significant and the compound of interest might be treated as more
hazardous than it actually is.

On the other hand, the compound of interest might be treated as less
hazardous if the mortality in the untreated control group is unusual
high. This might be the case, if its mortality exceeds 14.01781.

### Evaluation of several control groups

If a prediction interval for several future observations (in this case
several control groups from several trails) is needed, their group sizes
can be defined by `newsize`, eg. like `newsize=c(50, 30, 30, 30)`.

``` r
pi_m4 <- quasi_bin_pi(histdat=dat_real,
                      newsize=c(50, 30, 30, 30), 
                      traceplot = FALSE,
                      alpha=0.05)
pi_m4
#>   total hist_prob quant_calib  pred_se    lower    upper
#> 1    50     0.276    1.278262 8.854377 2.481788 25.11821
#> 2    30     0.276    1.278262 5.600000 1.121734 15.43827
#> 3    30     0.276    1.278262 5.600000 1.121734 15.43827
#> 4    30     0.276    1.278262 5.600000 1.121734 15.43827
```

In this case, the untreated control group that contains 50 animals is in
line with the historical control data if its mortality falls between
2.48179 and 25.11821. Similarly, the control groups that contain 30
animals are in line with the historical knowledge if their mortality
ranges between 1.12173 and 15.43827.

## References

Menssen, M., Schaarschmidt, F.: Prediction intervals for all of M future
observations based on linear random effects models. Statistica
Neerlandica. 2021. [DOI:
10.1111/stan.12260](https://onlinelibrary.wiley.com/doi/10.1111/stan.12260?af=R)

Menssen M, Schaarschmidt F.: Prediction intervals for overdispersed
binomial data with application to historical controls. Statistics in
Medicine. 2019;38:2652-2663.
[DOI:10.1002/sim.8124](https://onlinelibrary.wiley.com/doi/10.1002/sim.8124)

NTP 2017: [Tables of historical controls: pathology tables by
route/vehicle.](https://ntp.niehs.nih.gov/results/dbsearch/historical/index.html),
Accessed May 17, 2017.
