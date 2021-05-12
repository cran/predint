
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
    model

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
computed in order to validate the observed mortality of an actual (or
future) trial. In this scenario prediction intervals can be computed for
two different purposes.  
On the one hand, a PI for one future observation can be computed in
order to validate the outcome of one actual (or future) untreated
control group that is compared with several groups treated with the
compound of interest.  
On the other hand, in some cases it might be useful to validate the
outcome of the complete actual (or future) study including the treatment
groups, based on the knowledge gained from historical control data.  
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

# data set (Table 1 of the supplementary material of Menssen and Schaarschmidt 2019)
dat_real <- data.frame("dead"=c(15, 10, 12, 12, 13, 11, 19, 11, 14, 21),
                       "alive"=c(35, 40, 38, 38, 37, 39, 31, 39, 36, 29))

# PI for one future control group comprised of 50 mice
pi_m1 <- quasi_bin_pi(histdat=dat_real, 
                      newsize=30,
                      traceplot = FALSE, 
                      alpha=0.05)
pi_m1
#>   total hist_prob quant_calib pred_se   lower    upper
#> 1    30     0.276    1.014854     5.6 2.59682 13.96318
```

The historical binomial probability of success (historical mortality
rate) is 0.276, the bootstrap calibrated coefficient is 1.01485 and the
standard error of the prediction is 5.6. The lower limit of the
bootstrap calibrated asymptotic prediction interval is 2.59682 and its
upper limit is given by 13.96318.

If the mortality is lower than 2.59682 (practically spoken lower than 3)
it can be treated as unusual low. Consequently, mean comparisons between
the control group might result in too many differences that are
considered as significant and the compound of interest might be treated
as more hazardous than it actually is.  
On the other hand, the compound of interest might be treated as less
hazardous if the mortality in the untreated control group is unusual
high. This might be the case, if its mortality exceeds 13.96318
(practically spoken higher than 13).

### Evaluation of one future study

Let us assume, there is a study in which one untreated control group
comprised of 50 male mice is compared to three treatment groups
comprised of 30 mice each. If the whole study should be compared with
the historical knowledge, four prediction intervals have to be computed.
Hence `newsize` is set to `c(50, 30, 30, 30)`.

``` r
pi_m4 <- quasi_bin_pi(histdat=dat_real,
                      newsize=c(50, 30, 30, 30), 
                      traceplot = FALSE,
                      alpha=0.05)
pi_m4
#>   total hist_prob quant_calib  pred_se    lower    upper
#> 1    50     0.276    1.288018 8.854377 2.395406 25.20459
#> 2    30     0.276    1.288018 5.600000 1.067102 15.49290
#> 3    30     0.276    1.288018 5.600000 1.067102 15.49290
#> 4    30     0.276    1.288018 5.600000 1.067102 15.49290
```

In this case, the untreated control group is in line with the historical
control data if its mortality falls between 2.39541 and 25.20459.
Similarly, the groups treated with the compound of interest are in line
with the historical knowledge regarding untreated control groups if
their mortality ranges between 1.0671 and 15.4929. This means that the
compound of interest might not have an effect on mortality, if the
observed moralities of some (or all) of the treatment groups fall into
their corresponding prediction interval.

## References

Menssen M, Schaarschmidt F.: Prediction intervals for overdispersed
binomial data with application to historical controls. Statistics in
Medicine. 2019;38:2652-2663. <https://doi.org/10.1002/sim.8124>

NTP 2017: Tables of historical controls: pathology tables by
route/vehicle.
<https://ntp.niehs.nih.gov/results/dbsearch/historical/index.html>.
Accessed May 17, 2017.
