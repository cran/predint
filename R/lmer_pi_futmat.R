
#' Prediction intervals for future observations based on linear random effects models
#'
#' \code{lmer_pi_futmat()} calculates a bootstrap calibrated prediction interval for one or more
#' future observation(s) based on linear random effects models. With this approach,
#' the experimental design of the future data is taken into account (see below).
#'
#' @param model a random effects model of class  \code{"lmerMod"}
#' @param newdat either 1 or a \code{data.frame} with the same column names as the historical data
#' on which \code{model} depends
#' @param futmat_list a list that contains design matrices for each random factor
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
#' If \code{newdat} is defined, the bootstrapped future observations used for the calibration
#' process mimic the structure of the data set provided via \code{newdat}. The
#' data sampling is based on a list of design matrices (one for each random factor)
#' that can be obtained if \code{newdat} and the model formula are provided to
#' \code{lme4::lFormula()}. Hence, each random factor that is part of the initial
#' model must have at least two replicates in \code{newdat}. \cr
#' If a random factor in the future data set does not have any replicate, a list
#' that contains design matrices (one for each random factor) can be provided via \code{futmat_list}.
#'
#' This function is an implementation of the PI given in Menssen and Schaarschmidt 2022
#' section 3.2.4, except, that the bootstrap calibration values are drawn from
#' bootstrap samples that mimic the future data as described above.
#'
#'
#' @return \code{lmer_pi_futmat()} returns an object of class \code{c("predint", "normalPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#' @export
#'
#' @importFrom graphics abline lines
#' @importFrom lme4 fixef VarCorr bootMer lFormula
#' @importFrom stats vcov rnorm var
#' @importFrom methods is
#'
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
#' \donttest{fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)}
#' \donttest{summary(fit)}
#'
#' #----------------------------------------------------------------------------
#' ### Using newdat
#'
#' # Prediction interval using c2_dat2 as future data
#' \donttest{pred_int <- lmer_pi_futmat(model=fit, newdat=c2_dat2, alternative="both", nboot=100)
#' summary(pred_int)}
#'
#' # Upper prediction limit for m=1 future observations
#' \donttest{pred_u <- lmer_pi_futmat(model=fit, newdat=1, alternative="upper", nboot=100)
#' summary(pred_u)}
#'
#' #----------------------------------------------------------------------------
#'
#' ### Using futmat_list
#'
#' # c2_dat4 has no replication for b. Hence the list of design matrices can not be
#' # generated by lme4::lFormula() and have to be provided by hand via futmat_list.
#'
#' c2_dat4
#'
#' # Build a list containing the design matrices
#'
#' fml <- vector(length=4, "list")
#'
#' names(fml) <- c("a:b", "b", "a", "Residual")
#'
#' fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))
#'
#' fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["Residual"]] <- diag(6)
#'
#' fml
#'
#' # Please note, that the design matrix for the interaction term a:b is also
#' # provided even there is no replication for b, since it is assumed that
#' # both, the historical and the future data descent from the same data generating
#' # process.
#'
#' # Calculate the PI
#' \donttest{pred_fml <- lmer_pi_futmat(model=fit, futmat_list=fml, alternative="both", nboot=100)
#' summary(pred_fml)}
#'
#' #----------------------------------------------------------------------------
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
lmer_pi_futmat <- function(model,
                           newdat=NULL,
                           futmat_list=NULL,
                           alternative="both",
                           alpha=0.05,
                           nboot=10000,
                           delta_min=0.01,
                           delta_max=10,
                           tolerance = 1e-3,
                           traceplot=TRUE,
                           n_bisec=30,
                           algorithm="MS22"){

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


        # First substitue all whitespace characters with nothing ("") to make shure they don't disturb.
        # '\\s' is regex for 'all whitespace characters like space, tab, line break, ...)'
        fs <- gsub("\\s", "", fs)

        # Are there any occurances where '|' is not preceded by a '1'?
        # '[^1]' is regex for 'not 1' and '\\|' is just regex for '|'.
        wrong_formula <- grepl('[^1]\\|', fs)

        if(wrong_formula){
                stop("Random effects must be specified as (1|random_effect)")
        }

        #-----------------------------------------------------------------------
        ### Actual data

        if(is.null(newdat) & is.null(futmat_list)){
                stop("newdat and futmat_list are both NULL")
        }

        if(!is.null(newdat) & !is.null(futmat_list)){
                stop("newdat and futmat_list are both defined")
        }

        ### newdat
        if(!is.null(newdat)){
                # newdat needs to be a data.frame or 1
                if(is.data.frame(newdat)==FALSE){

                        if(length(newdat) != 1){
                                stop("newdat has to be a data.frame or equal 1")
                        }

                        else if(newdat != 1){
                                stop("newdat has to be a data.frame or equal 1")
                        }
                }

                # Check conditions if newdat is a data frame
                if(is.data.frame(newdat)){

                        # colnames of historical data and new data must be the same
                        if(all(colnames(model@frame) == colnames(newdat))==FALSE){
                                stop("colnames(model@frame) and colnames(newdat) are not the same.\nHave you transformed the depenent variable within the lmer() call?\nAt their current stage, the lmer_pi_... functions do not work with\ntransformations inside lmer()")
                        }
                }
        }

        ### futmat_list
        else if(!is.list(futmat_list)){
                stop("futmat_list needs to be a list that contains the design matrices for each random effect")
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

        # SE for the prediction
        pred_se_hat <- sqrt(sum(c(as.vector(vcov(model)),
                                  data.frame(VarCorr(model))$vcov)))

        #----------------------------------------------------------------------
        ### Bootstrapping of future observations

        # If newdat is defined
        if(!is.null(newdat) & is.null(futmat_list)){

                # If newdat=1
                if(is.data.frame(newdat)==FALSE){

                        # Extracting the observations
                        obs_fun <- function(.){
                                bs_dat <- .@frame[,1]
                        }

                        # Bootstrap for the observations
                        boot_obs <- bootMer(model, obs_fun, nsim = nboot)

                        # Bootstrapped data sets
                        bsdat_list <- as.list(as.data.frame(t(boot_obs$t)))

                        # Take only m random observation per data set
                        ystar_fun <- function(.){
                                y_star <- sample(x=., size=1)

                                y_star_min <- min(y_star)
                                y_star_max <- max(y_star)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }


                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)

                        # define number of fut. observations
                        m_fut <- length(unique(unlist(ystar_list[[1]])))

                }

                # If newdat is a data.frame
                else{
                        # SD for the random factors
                        sd_hist <- as.data.frame(VarCorr(model))[,c("grp", "sdcor")]

                        # Model Frame for the new data without fitting the data
                        modelframe_list <- lFormula(eval(model), data=newdat)

                        # Intercept (fixed effect)
                        mu <- matrix(unname(modelframe_list$X * fixef(model)))

                        # number of future observations
                        n_fut <- length(modelframe_list$X)

                        # Random effects matrix
                        Zt_list <- modelframe_list$reTrms$Ztlist

                        # Names of Zt_list should be the same as for the SD in sd_hist
                        names(Zt_list) <- gsub("\\s", "", gsub("[1|]", "", names(Zt_list)))

                        # Design Matrix for random effects with residuals
                        Zt_list <- lapply(X=Zt_list, as.matrix)
                        Z_list <- c(lapply(X=Zt_list, FUN=t), Residual=list(diag(1, nrow=n_fut)))

                        # Sampling of B bootstrap samples
                        bsdat_list <- vector(length=nboot, "list")

                        for(b in 1:length(bsdat_list)){

                                # Sampling of random effects
                                u_list <- vector("list",nrow(sd_hist))
                                names(u_list) <- names(Z_list)

                                for(c in 1:length(u_list)){
                                        u_c <- rnorm(n=ncol(Z_list[[c]]), mean=0, sd=sd_hist[c, 2])
                                        u_list[[c]] <- u_c
                                }

                                # Random effects times design matrix
                                ZU_list <- Map(function(x, y)  x%*%y, Z_list, u_list)
                                ZU_list[["mu"]] <- mu

                                # Bootstrap data
                                bsdat_list[[b]] <- rowSums(matrix(unlist(ZU_list), ncol = length(ZU_list)))

                        }

                        # Take the min and the max of the M future observations
                        ystar_fun <- function(.){
                                # y_star <- sample(x=., size=m)

                                y_star_min <- min(.)
                                y_star_max <- max(.)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }

                        # define number of fut. observations
                        m_fut <- length(bsdat_list[[1]])

                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)


                }

        }

        #-----------------------------------------------------------------------

        ### Bootstrapping future observations if futmat_list is given

        if(!is.null(futmat_list) & is.null(newdat)){

                # SD for the random factors
                sd_hist <- as.data.frame(VarCorr(model))[,c("grp", "sdcor")]

                if(length(names(futmat_list)) != length(sd_hist$grp)){
                        stop("length(names(futmat_list)) is not the same as the number of random factors plus the residuals")
                }

                if(!all(unlist(lapply(X=futmat_list, FUN=is.matrix)))){
                        stop("all elemts of futmat_list have to be a matrix")
                }

                # all matrices should contain only 1 and 0
                ozfun <- function(mat){all(mat %in% c(0, 1))}
                if(!all(unlist(lapply(futmat_list, ozfun)))){
                        stop("all matrices should contain only 1 and 0")
                }

                if(var(unname(unlist(lapply(X=futmat_list, FUN=nrow)))) != 0){
                        stop("all matrices need to have the same numbers of rows")
                }

                if(all(sd_hist$grp == names(futmat_list))==FALSE){
                        stop("futmat_list needs to have the same names as the random factors in the model. Maybe you forgot the residuals?")
                }

                else{
                        # Intercept (fixed effect)
                        mu <- matrix(1, nrow(futmat_list[[1]])) * fixef(model)

                        Z_list <- futmat_list

                        # Sampling of B bootstrap samples
                        bsdat_list <- vector(length=nboot, "list")

                        for(b in 1:length(bsdat_list)){

                                # Sampling of random effects
                                u_list <- vector("list",nrow(sd_hist))
                                names(u_list) <- names(Z_list)

                                for(c in 1:length(u_list)){

                                        u_c <- rnorm(n=ncol(Z_list[[c]]), mean=0,
                                                     sd=sd_hist[c, 2])
                                        u_list[[c]] <- u_c
                                }

                                # Random effects times design matrix
                                ZU_list <- Map(function(x, y)  x%*%y, Z_list, u_list)
                                ZU_list[["mu"]] <- mu

                                # Bootstrap data
                                bsdat_list[[b]] <- rowSums(matrix(unlist(ZU_list),
                                                                  ncol = length(ZU_list)))

                        }

                        # Take only m random observation per data set
                        ystar_fun <- function(.){
                                # y_star <- sample(x=., size=m)

                                y_star_min <- min(.)
                                y_star_max <- max(.)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }

                        # define number of fut. observations
                        m_fut <- length(bsdat_list[[1]])

                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)
                }
        }


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
                         pred_se=pred_se_hat,
                         m=m_fut,
                         q=quant_calib,
                         alternative=alternative,
                         newdat=newdat,
                         futmat_list=futmat_list,
                         histdat=model@frame,
                         algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)

}

















