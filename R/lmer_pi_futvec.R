
#' Prediction intervals for future observations based on linear random effects models
#'
#' \code{lmer_pi_futvec()} calculates a bootstrap calibrated prediction interval for one or more
#' future observation(s) based on linear random effects models. With this approach,
#' the experimental design of the future data is taken into account (see below).
#'
#' @param model a random effects model of class lmerMod
#' @param newdat a \code{data.frame} with the same column names as the historical data
#' on which  \code{model} depends
#' @param futvec an integer vector that defines the structure of the future data based on the
#' row numbers of the historical data. If \code{length(futvec)} is one, a PI
#' for one future observation is computed
#' @param alternative either "both", "upper" or "lower". \code{alternative} specifies
#' if a prediction interval or an upper or a lower prediction limit should be computed
#' @param alpha defines the level of confidence (1-\code{alpha})
#' @param nboot number of bootstraps
#' @param delta_min lower start value for bisection
#' @param delta_max upper start value for bisection
#' @param tolerance tolerance for the coverage probability in the bisection
#' @param traceplot if \code{TRUE}: Plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#' @param algorithm either "MS22" or "MS22mod" (see details)
#'
#' @details This function returns bootstrap-calibrated prediction intervals as well as
#' lower or upper prediction limits.
#'
#' If \code{algorithm} is set to "MS22", both limits of the prediction interval
#' are calibrated simultaneously using the algorithm described in Menssen and
#' Schaarschmidt (2022), section 3.2.4. The calibrated prediction interval is given
#' as
#'
#' \deqn{[l,u] = \hat{\mu} \pm q^{calib} \sqrt{\widehat{var}(\hat{\mu}) + \sum_{c=1}^{C+1}
#' \hat{\sigma}^2_c}}
#'
#' with \eqn{\hat{\mu}} as the expected future observation (historical mean) and
#' \eqn{\hat{\sigma}^2_c} as the \eqn{c=1, 2, ..., C} variance components and \eqn{\hat{\sigma}^2_{C+1}}
#' as the residual variance obtained from the random
#' effects model fitted with \code{lme4::lmer()} and \eqn{q^{calib}} as the as the bootstrap-calibrated
#' coefficient used for interval calculation. \cr
#'
#' If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
#' are calibrated independently from each other. The resulting prediction interval
#' is given by
#'
#' \deqn{[l,u] = \Big[\hat{\mu} - q^{calib}_l \sqrt{\widehat{var}(\hat{\mu}) + \sum_{c=1}^{C+1} \hat{\sigma}^2_c}, \quad
#' \hat{\mu} + q^{calib}_u \sqrt{\widehat{var}(\hat{\mu}) + \sum_{c=1}^{C+1} \hat{\sigma}^2_c} \Big].}
#'
#' Please note, that this modification does not affect the calibration procedure, if only
#' prediction limits are of interest. \cr
#'
#' Be aware that the sampling structure of the historical data must contain the structure of the
#' future data. This means that the observations per random factor must be less or
#' equal in the future data compared to the historical data.
#'
#' This function is an implementation of the PI given in Menssen and Schaarschmidt 2022 section 3.2.4
#' except that the bootstrap calibration values are drawn from bootstrap samples that
#' mimic the future data.
#'
#' @return \code{lmer_pi_futvec()} returns an object of class \code{c("predint", "normalPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#' @export
#'
#' @importFrom graphics abline lines
#' @importFrom lme4 fixef VarCorr bootMer
#' @importFrom stats vcov
#' @importFrom stats na.omit
#' @importFrom methods is
#'
#' @references Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica,
#'  \doi{10.1111/stan.12260}
#'
#' @examples
#'
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' #----------------------------------------------------------------------------
#'
#' ### Prediction interval using c2_dat3 as future data
#' # without printing c2_dat3 in the output
#'
#' # Row numbers of the historical data c2_dat1 that define the structure of
#' # the future data c2_dat3
#' futvec <- c(1, 2, 4, 5, 10, 11, 13, 14)
#'
#' # Calculating the PI
#' \donttest{pred_int <- lmer_pi_futvec(model=fit, futvec=futvec, nboot=100)
#' summary(pred_int)}
#'
#' #----------------------------------------------------------------------------
#'
#' ### Calculating the PI with c2_dat3 printed in the output
#' \donttest{pred_int_new <- lmer_pi_futvec(model=fit, futvec=futvec, newdat=c2_dat3, nboot=100)
#' summary(pred_int_new)}
#'
#' #----------------------------------------------------------------------------
#'
#' ### Upper prediction limit for m=1 future observation
#' \donttest{pred_u <- lmer_pi_futvec(model=fit, futvec=1, alternative="upper", nboot=100)
#' summary(pred_u)}
#'
#'#----------------------------------------------------------------------------
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
lmer_pi_futvec <- function(model,
                           futvec,
                           newdat=NULL,
                           alternative="both",
                           alpha=0.05,
                           nboot=10000,
                           delta_min=0.01,
                           delta_max=10,
                           tolerance = 1e-3,
                           traceplot=TRUE,
                           n_bisec=30,
                           algorithm="MS22"){

        # warning("This function needs some work.")

        # Model must be of class lmerMod
        if(!is(model, "lmerMod")){
                stop("class(model) != lmerMod")
        }

        # Model must be a random effect model
        if(length(fixef(model)) != 1){
                stop("length(fixef(model)) must be 1 (the model must be a random effects model)")

        }


        ### All random effects must be specified as (1|random_effect)
        # Get forumla object
        f <- model@call$formula

        # Right part of formula as a string
        fs <- as.character(f)[3]


        # First substitue all whitespace characters with nothing ("") to make sure they don't disturb.
        # '\\s' is regex for 'all whitespace characters like space, tab, line break, ...)'
        fs <- gsub("\\s", "", fs)

        # Are there any occurrences where '|' is not preceded by a '1'?
        # '[^1]' is regex for 'not 1' and '\\|' is just regex for '|'.
        wrong_formula <- grepl('[^1]\\|', fs)

        if(wrong_formula){
                stop("Random effects must be specified as (1|random_effect)")
        }

        ### futvec

        if((is.numeric(futvec) | is.integer(futvec))==FALSE){
                stop("futvec needs to be an integer or numeric vector that contains only integer values")
        }
        # are some elements the same
        if(length(unique(futvec)) != length(futvec)){
                stop("some of the elements in futvec are equal")
        }

        # all elements must be integer values
        if(!isTRUE(all(futvec == floor(futvec)))){
                stop("futvec must only contain integer values")
        }

        # futvec is not allowed to be longer than histdat
        if(length(futvec) > nrow(model@frame)){
                stop("length(futvec) > nrow(histdat): futvec and historical data do not match")
        }

        # The max of futvec can not be higher than nrow histdat
        if(max(futvec) > nrow(model@frame)){
                stop("max(futvec) > nrow(histdat): futvec and historical data do not match")
        }

        # min of futvec must be at least 1
        if(min(futvec) < 1){
                stop("min(futvec) must at least 1")
        }

        # futvec can not contain any NA
        if(length(futvec) != length(na.omit(futvec))){
                stop("futvec contains at least one NA")
        }



        ### Actual data
        if(is.null(newdat) == FALSE){

                # newdat needs to be a data.frame
                if(is.data.frame(newdat)==FALSE){
                        stop("newdat is not a data.frame")
                }

                # colnames of historical data and new data must be the same
                if(all(colnames(model@frame) == colnames(newdat))==FALSE){
                        stop("colnames(model@frame) and colnames(newdat) are not the same.\nHave you transformed the depenent variable within the lmer() call?\nAt their current stage, the lmer_pi_... functions do not work with\ntransformations inside lmer()")
                }

                #
                if(length(futvec) != nrow(newdat)){
                        stop("length(futvec) != nrow(newdat)")
                }

                warning("NOTE: The elements of futvec must reflect the structure of newdat in histdat")


        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        #-----------------------------------------------------------------------

        # algorithm must be set properly
        if(algorithm != "MS22"){
                if(algorithm != "MS22mod"){
                        stop("algoritm must be either MS22 of MS22mod")
                }
        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        #----------------------------------------------------------------------

        # Extraction of the intercept
        mu_hat <- unname(fixef(model))

        # SE for the future observation
        se_y_star_hat <- sqrt(sum(c(as.vector(vcov(model)),
                                    data.frame(VarCorr(model))$vcov)))

        #----------------------------------------------------------------------
        ### Bootstrapping future observations

        # Extracting the observations
        obs_fun <- function(.){
                bs_dat <- .@frame[,1]
        }

        # Bootstrap for the observations
        boot_obs <- bootMer(model, obs_fun, nsim = nboot)

        # # Smallest BS observation
        # bs_y_min <- min(t(boot_obs$t))
        #
        # # Biggest BS observation
        # bs_y_max <- max(t(boot_obs$t))

        # Bootstrapped data sets
        bsdat_list <- as.list(as.data.frame(t(boot_obs$t)))

        # Take only m random observation per data set
        ystar_fun <- function(.){

                if(length(futvec)==1){
                        y_star <- sample(x=., size=1)

                        y_star_min <- min(y_star)
                        y_star_max <- max(y_star)

                        c("y_star_min"=y_star_min,
                          "y_star_max"=y_star_max)
                }

                else{
                        y_star <- .[futvec]

                        y_star_min <- min(y_star)
                        y_star_max <- max(y_star)

                        c("y_star_min"=y_star_min,
                          "y_star_max"=y_star_max)
                }
        }

        # List with future observations (y_star)
        ystar_list <- lapply(bsdat_list, ystar_fun)

        #----------------------------------------------------------------------
        ### Bootstrapping the variance of y_star

        # Function to get se(y_star)
        se_fun <- function(.){
                bs_var_y_star <- sum(c(as.vector(vcov(.)), data.frame(VarCorr(.))$vcov))
                bs_se_y_star <- sqrt(bs_var_y_star)

                bs_mu <- unname(fixef(.))

                c(bs_mu=bs_mu, bs_se_y_star=bs_se_y_star)
        }

        # Bootstrap
        boot_se <- bootMer(model, se_fun, nsim = nboot)

        # Bootstrapped Parameters
        bs_params <- data.frame(boot_se$t)

        # Bootstrapped se
        bs_se<- as.list(as.vector(bs_params$bs_se_y_star))

        # Bootstrapped mu
        bs_mu<- as.list(as.vector(bs_params$bs_mu))



        #-----------------------------------------------------------------------

        ### Calculation of the calibrated quantile

        if(alternative=="lower"){

                quant_calib <- bisection(y_star_hat = bs_mu,
                                         pred_se = bs_se,
                                         y_star = ystar_list,
                                         alternative = alternative,
                                         quant_min = delta_min,
                                         quant_max = delta_max,
                                         n_bisec = n_bisec,
                                         tol = tolerance,
                                         alpha = alpha,
                                         traceplot=traceplot)
        }

        # Calibration for of upper prediction limits
        if(alternative=="upper"){

                quant_calib <- bisection(y_star_hat = bs_mu,
                                         pred_se = bs_se,
                                         y_star = ystar_list,
                                         alternative = alternative,
                                         quant_min = delta_min,
                                         quant_max = delta_max,
                                         n_bisec = n_bisec,
                                         tol = tolerance,
                                         alpha = alpha,
                                         traceplot=traceplot)
        }


        # Calibration for  prediction intervals
        if(alternative=="both"){

                # Direct implementation of M and S 2021
                if(algorithm=="MS22"){
                        quant_calib <- bisection(y_star_hat = bs_mu,
                                                 pred_se = bs_se,
                                                 y_star = ystar_list,
                                                 alternative = alternative,
                                                 quant_min = delta_min,
                                                 quant_max = delta_max,
                                                 n_bisec = n_bisec,
                                                 tol = tolerance,
                                                 alpha = alpha,
                                                 traceplot=traceplot)
                }

                # Modified version of M and S 21
                if(algorithm=="MS22mod"){
                        quant_calib_lower <- bisection(y_star_hat = bs_mu,
                                                       pred_se = bs_se,
                                                       y_star = ystar_list,
                                                       alternative = "lower",
                                                       quant_min = delta_min,
                                                       quant_max = delta_max,
                                                       n_bisec = n_bisec,
                                                       tol = tolerance,
                                                       alpha = alpha/2,
                                                       traceplot=traceplot)

                        quant_calib_upper <- bisection(y_star_hat = bs_mu,
                                                       pred_se = bs_se,
                                                       y_star = ystar_list,
                                                       alternative = "upper",
                                                       quant_min = delta_min,
                                                       quant_max = delta_max,
                                                       n_bisec = n_bisec,
                                                       tol = tolerance,
                                                       alpha = alpha/2,
                                                       traceplot=traceplot)

                        quant_calib <- c(quant_calib_lower, quant_calib_upper)
                }

        }

        #-----------------------------------------------------------------------
        ### Define the output object

        out <- normal_pi(mu=mu_hat,
                         pred_se=se_y_star_hat,
                         m=length(futvec),
                         q=quant_calib,
                         alternative=alternative,
                         newdat=newdat,
                         histdat=model@frame,
                         futvec=futvec,
                         algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)

}

