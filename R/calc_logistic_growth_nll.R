#' Calculate the negative log-likelihood of the pulsed logistic growth model
#'
#' @description You can use this function to fit a logistic growth model to
#'    experimental data assuming that you replaced media regularly to count
#'    the organism density. Fitting a regular logistic growth curve would
#'    create a bias, at least on the carrying capacity as the researcher
#'    disturbed the system and acts as "top predator".
#'    You can include different disturbance strength, e.g. replacing a fraction
#'    of e.g. 0.1 of the media at some time steps, but a fraction of 0.2 on
#'    other sampling dates.
#'    You can add replicates, but as the starting density is currently a
#'    parameter of the model, this density must be fixed across replicates!
#'    I hope to make this more flexible in the future to allow for different
#'    starting densities.
#'
#' @references TBA
#'
#' @useDynLib dynasymfitr
#' @import foreach
#' @importFrom odin odin
#' @importFrom stats dnorm
#' @importFrom stats dpois
#'
#'
#' @param data_time integer or float; a vector of time points at which the
#'     sampling took place.
#' @param data_density integer or float; a vector of organism densities at which
#'     the sampling took place (must match the length of data_time!).
#' @param data_frac_exchanged float; the fraction of the population that is
#'     removed at each sampling. Most likely this is "0" for the first (initial)
#'     sampling time point, but takes other values for the rest of the time
#'     series (must match the length of data_time!)..
#' @param data_ts_id character or factor (should also work with numbers). The
#'     identifier of each time series. This is crucial as computed time steps
#'     may differ across time series, e.g. a on replicate is sampled every day,
#'     the other is sampled every other day. This would lead to different biases
#'     regarding realized growth and maximum density is reached,
#' @param setting_t_length  integer or float; the number of time steps that
#'     should be simulated between the sampling. The more time steps, the more
#'     precise the simulation. A single value; default = 100.
#' @param param_r_log10 float; the log10 of the intrinsic growth rate. A single value.
#' @param param_K_log10 float; the log10 of the carrying capacity. A single value.
#' @param param_sd_log10 float; the log10 of the standard deviation. A single value.
#' @param param_n_initial_log10 the log10 of the initial resource density.
#'
#'
#' @return Returns a single negative log-likelihood value.
#'
#' @export
#'
#' @examples
#'
#' library("bbmle")
#' library("dynasymfitr")
#'
#' ## Example for fitting a time series with log normal data distribution
#' ## This example assumes very high densities, floating numbers, counts possible,
#' ## no zeros (as log would no work then)
#'
#' # simulate some data:
#' set.seed(667)
#'
#' data_log_growth <- simulate_logistic_growth_pulsed(
#'   t_pulse = 0:20,                           # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = c(0, rep(0.1,20)),   # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 1000,                            # the initial resource density
#'   r = .8,                                   # the intrinsic growth rate
#'   K = 100000                                   # the carrying capacity
#' )
#' # create a second time series:
#' data_log_growth <- rbind(data_log_growth, data_log_growth)
#'
#' # Add an ID for each time series (required!!!)
#' data_log_growth$ts_id <- c(rep("A",21), rep("B",21))
#'
#' # Add information at which timesteps a disturbance happend (not at t = 0!)
#' data_log_growth$frac_replaced <- c(0, rep(0.1,20), 0, rep(0.1,20))
#'
#' # add statistical noise (poisson distributed)
#' data_log_growth$n_noise <- 10^(rnorm(n = length(data_log_growth$n), mean = log10(data_log_growth$n), sd = .05 ))
#'
#' # plot the time series
#' par(las=1)
#' plot(
#'   data_log_growth$t,
#'   data_log_growth$n_noise,
#'   ylim = c(0, 125000),
#'   type = "n",
#'   pch = 16,
#'   xlab = "time",
#'   ylab = "density"
#' )
#' points(
#'   data_log_growth$t[data_log_growth$ts_id == "A"],
#'   data_log_growth$n_noise[data_log_growth$ts_id == "A"],
#'   pch = 16,
#'   col = "blue"
#' )
#' points(
#'   data_log_growth$t[data_log_growth$ts_id == "B"],
#'   data_log_growth$n_noise[data_log_growth$ts_id == "B"],
#'   pch = 16,
#'   col = "red"
#' )
#'
#'
#'
#' fit_lnorm <- bbmle::mle2(
#'   minuslogl = calc_logistic_growth_nll_lnorm,
#'   start = list(
#'     param_r_log10 = log10(0.8),
#'     param_K_log10 = log10(100000),
#'     param_n_initial_log10 = log10(1000),
#'     param_sd_log10 = log10(0.1)
#'   ),
#'   data = list(
#'     data_time = data_log_growth$t,
#'     data_density = data_log_growth$n_noise,
#'     data_frac_exchanged = data_log_growth$frac_replaced,
#'     data_ts_id = data_log_growth$ts_id
#'   ),
#'   fixed = list(
#'     setting_t_length = 1000
#'   ),
#'   control = list(reltol = 1e-12, maxit = 1000)
#' )
#'
#' bbmle::summary(fit_lnorm)
#'
#' # growth rate simulated value (0.8) is well-matched by fit:
#' 10^bbmle::summary(fit_lnorm)@coef[1]
#'
#' # carrying capacity simulated value (100000) is well-matched by fit:
#' 10^bbmle::summary(fit_lnorm)@coef[2]
#'
#' # standard distribution of data (0.05) is well-matched by fit:
#' 10^bbmle::summary(fit_lnorm)@coef[3]
#'
#' # initial start value of data (1000) is well-matched by fit:
#' 10^bbmle::summary(fit_lnorm)@coef[4]
#'
#'
#' ## create the regression line
#' reg_log_growth <- simulate_logistic_growth_pulsed(
#'   t_pulse = 0:20,                                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = c(0, rep(0.1,20)),           # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 10^bbmle::summary(fit_lnorm)@coef[4], # the initial resource density
#'   r = 10^bbmle::summary(fit_lnorm)@coef[1],         # the intrinsic growth rate
#'   K = 10^bbmle::summary(fit_lnorm)@coef[2]          # the carrying capacity
#' )
#' lines(reg_log_growth$t, reg_log_growth$n, lwd = 2)
#'
#' ## simulate the time series assuming no sampling:
#' reg_log_growth2 <- simulate_logistic_growth_pulsed(
#'   t_pulse = seq(0, 20, length = 1000),              # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = rep(0,1000),                # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 10^bbmle::summary(fit_lnorm)@coef[4], # the initial resource density
#'   r = 10^bbmle::summary(fit_lnorm)@coef[1],         # the intrinsic growth rate
#'   K = 10^bbmle::summary(fit_lnorm)@coef[2]          # the carrying capacity
#' )
#' lines(reg_log_growth2$t, reg_log_growth2$n, lwd = 2, col = "red", lty = 2)
#'
#'


calc_logistic_growth_nll_lnorm <- function(
    data_time,
    data_density,
    data_frac_exchanged,
    data_ts_id,
    setting_t_length = 100,
    param_r_log10,
    param_K_log10,
    param_sd_log10,
    param_n_initial_log10
){


  ## all data into on dataframe for easier handling
  df <- data.frame(
    t_pulse = data_time,
    n_pulse = data_density,
    frac_exchanged = data_frac_exchanged,
    ts_id = data_ts_id
  )

  nll <- Inf

  lls <- foreach::foreach(
    i = 1:length(unique(df$ts_id)),
    .combine = "c") %do% {

    dfi <- subset(df, ts_id == unique(df$ts_id)[i])
    lls_i <- -Inf

    try({
      out <- simulate_logistic_growth_pulsed(
        t_pulse = dfi$t_pulse,
        fraction_exchanged = dfi$frac_exchanged,
        n_initial = 10^param_n_initial_log10,
        r = 10^param_r_log10,
        K = 10^param_K_log10,
        t_length = setting_t_length,
        return_all_steps = FALSE
      )

      lls_i <- stats::dnorm(
        x = log10(dfi$n_pulse),
        mean = log10(out$n),
        sd = 10^param_sd_log10,
        log = T
      )

      rm(out)

    }, silent = TRUE)
    return(lls_i)
  }

  nll <- -1*sum(lls)

  if(is.na(nll)){return(Inf)}

  return(nll)
}

