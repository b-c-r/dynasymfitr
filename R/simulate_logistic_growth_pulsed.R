#' simulate_logistic_growth_pulsed: simulates a time series assuming logistic
#'     growth and pulsed sampling
#'
#' @description `simulate_logistic_growth_pulsed` simulates a time series
#'     assuming logistic growth and pulsed sampling / media exchange as often
#'     performed in aquatic lab experiments as alternative to chemostats.
#'
#' @useDynLib dynasymfitr
#' @importFrom odin odin
#'
#' @param t_pulse integer or float; a vector of time points at which the pulse
#'     (exchange of media) happens.
#' @param fraction_exchanged float; The fraction of the population that is
#'     removed at t_pulse. A vector of values ranging from 0-1 (0% to 100%).
#'     Must match the length of t_pulse.
#' @param n_initial integer or float; a vector of initial resource densities.
#' @param r float; the intrinsic growth rate. A single value.
#' @param K float; the carrying capacity. A single value.
#' @param t_length integer or float; the number of time steps that should be
#'     generated between the pulses. The more time steps, the more precise the
#'     simulation. A single value; default = 1000.
#' @param return_all_steps logical; should intermediate steps be returned (TRUE)
#'     or only values at t_pulse (FALSE). Default to FALSE.
#'
#' @return Returns a data frame with time and the population density n.
#'
#' @export
#'
#' @examples
#'
#' ## Scenario 1: constant media exchange rate
#' # simulate the time series
#' # first, the time steps at which a measure should be taken
#'
#' df_in <- data.frame(
#'   t_pulse = seq(0,40,2),
#'   fraction_exchanged = c(0, rep(0.1, 20))
#' )
#' # t_pulse: time points at which a measurement was taken, including the
#' #          initial time (time = 0), here 21 measures including initial
#' # fraction_exchanged: the fraction that is exchanged for measures at each
#' #                     "t_pulse"; should start with "0" at t_pulse = 0 as this
#' #                     start of the virtual experiment and media was not
#' #                     replaced. the number of values must match t_pulse
#' #                     entries: "0" plus 20 times "0.1".
#'
#' # Take a look at these input vectors, stored in a data frame:
#' str(df_in)
#' head(df_in)
#'
#' # simulate the output at t_pulse:
#' out <- simulate_logistic_growth_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 1000,                      # the initial resource density
#'   r = .4,                                # the intrinsic growth rate
#'   K = 10000                              # the carrying capacity
#' )
#'
#' # plot the time series
#' par(las=1)
#' plot(
#'   out$t,
#'   out$n,
#'   ylim = c(0, 10000),
#'   type = "p",
#'   pch = 16,
#'   xlab = "time",
#'   ylab = "density"
#' )
#'
#' out2 <- simulate_logistic_growth_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 1000,                      # the initial resource density
#'   r = .4,                                # the intrinsic growth rate
#'   K = 10000,                              # the carrying capacity
#'   return_all_steps = TRUE,               # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out2$t,
#'   out2$n
#' )
#' # note that the carrying capacity is never reached due to the disturbances.
#'
#' # a non-disturbed time series would look like:
#' out3 <- simulate_logistic_growth_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = rep(0, 21),       # no media replaced in this in-silico trial
#'   n_initial = 1000,                      # the initial resource density
#'   r = .4,                                # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   return_all_steps = TRUE,               # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out3$t,
#'   out3$n,
#'   col = "red",
#'   lty = 2,
#'   lwd = 2
#' )
#'
#' ## Scenario 2: differences in pulses/disturbances
#' # simulate the time series
#' # first, the time steps at which a measure should be taken
#'
#' df_in <- data.frame(
#'   t_pulse = seq(0,40,2),
#'   fraction_exchanged = c(0, runif(20, 0, 0.5))
#' )
#' # t_pulse: time points at which a measurement was taken, including the
#' #          initial time (time = 0), here 21 measures including initial
#' # fraction_exchanged: the fraction that is exchanged for measures at each
#' #                     "t_pulse"; should start with "0" at t_pulse = 0 as this
#' #                     start of the virtual experiment and media was not
#' #                     replaced. the number of values must match t_pulse
#' #                     entries: "0" plus 20 random disturbances.
#'
#' # Take a look at these input vectors, stored in a data frame:
#' str(df_in)
#' head(df_in)
#'
#' # simulate the output at t_pulse:
#' out <- simulate_logistic_growth_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 1000,                      # the initial resource density
#'   r = .4,                                # the intrinsic growth rate
#'   K = 10000                              # the carrying capacity
#' )
#'
#' # plot the time series
#' par(las=1)
#' plot(
#'   out$t,
#'   out$n,
#'   ylim = c(0, 10000),
#'   type = "p",
#'   pch = 16,
#'   xlab = "time",
#'   ylab = "density"
#' )
#'
#' out2 <- simulate_logistic_growth_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n_initial = 1000,                      # the initial resource density
#'   r = .4,                                # the intrinsic growth rate
#'   K = 10000,                              # the carrying capacity
#'   return_all_steps = TRUE,               # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out2$t,
#'   out2$n
#' )
#'

simulate_logistic_growth_pulsed <- function(
    t_pulse,
    fraction_exchanged,
    n_initial,
    r,
    K,
    t_length = 100,
    return_all_steps = FALSE
){

  i <- NULL

  t_out <- c(t_pulse[1])
  n_out <- c(n_initial)
  t_out_all <- c()
  n_out_all <- c()

  # for loop between t_pulse steps


  for(i in 1:length(t_pulse[-1])){
    ##
    logistic_growth_model_assigned <- logistic_growth_model$new(
      n_initial = n_out[i]*(1-fraction_exchanged[i]),
      r = r,
      K = K
    )
    ##
    tt <- seq(
      t_out[i],
      t_pulse[i+1],
      length.out = t_length
    )
    ##
    pop_dens <- logistic_growth_model_assigned$run(tt)
    ##
    t_out <- c(t_out, pop_dens[nrow(pop_dens),1])
    n_out <- c(n_out, pop_dens[nrow(pop_dens),2])
    t_out_all <- c(t_out_all, pop_dens[,1])
    n_out_all <- c(n_out_all, pop_dens[,2])
  }


  rm(i)

  # return the data frame

  if(return_all_steps){
    return_frame <- data.frame(
      t = t_out_all,
      n = n_out_all
    )
  } else{
    return_frame <- data.frame(
      t = t_out,
      n = n_out
    )
  }

  return(return_frame)

}
