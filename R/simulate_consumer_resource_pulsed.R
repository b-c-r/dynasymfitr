#' simulate_rma_pulsed: simulates a time series of a consumer
#'     resource model and pulsed sampling / disturbance
#'
#' @description `simulate_rma_pulsed` simulates a time series
#'     assuming a Rosenzweig-MacArthur type of a consumer resource model and
#'     pulsed sampling / media exchange (or disturbance) as often performed in
#'     aquatic lab experiments as alternative to chemostats.
#'
#' @useDynLib dynasymfitr
#' @importFrom odin odin
#'
#' @param t_pulse integer or float; a vector of time points at which the pulse
#'     (exchange of media) happens.
#' @param fraction_exchanged float; The fraction of the population that is
#'     removed at t_pulse. A vector of values ranging from 0-1 (0% to 100%).
#'     Must match the length of t_pulse.
#' @param n1_initial integer or float; a single value of initial resource densities.
#' @param n2_initial integer or float; a single value of initial consumer densities.
#' @param r float; the intrinsic growth rate. A single value.
#' @param K float; the carrying capacity. A single value.
#' @param e float; trophic level transfer rate, becomes assimilation efficiency for biomass/biovolume models
#' @param f_max float; maximum feeding rate
#' @param n_half float; half saturation density
#' @param q float; functional response shape paramter
#' @param m float; consumer's loss rate, can be death rate or metabolic rates, or both
#' @param t_length integer or float; the number of time steps that should be
#'     generated between the pulses. The more time steps, the more precise the
#'     simulation. A single value; default = 1000.
#' @param return_all_steps logical; should intermediate steps be returned (TRUE)
#'     or only values at t_pulse (FALSE). Default to FALSE.
#'
#' @return Returns a data frame with time and the population densities n and c.
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
#' out <- simulate_rma_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n1_initial = 100,                      # the initial resource density
#'   n2_initial = 2,                        # the initial consumer density
#'   r = 1,                                 # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   f_max = 6,                           # the maximum feeding rate
#'   n_half = 7500,                         # the half saturation density
#'   m = 2                               # the mortality rate
#' )
#'
#' # plot the time series
#' par(las=1)
#' plot(
#'   c(out$t,out$t),
#'   log10(c(out$n1,out$n2)),
#'   type = "n",
#'   pch = 16,
#'   xlab = "time",
#'   ylab = "log10-density"
#' )
#' points(
#'   out$t,
#'   log10(out$n1),
#'   col = "green",
#'   pch = 16
#' )
#' points(
#'   out$t,
#'   log10(out$n2),
#'   col = "blue",
#'   pch = 16
#' )
#'
#' out2 <- simulate_rma_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n1_initial = 100,                      # the initial resource density
#'   n2_initial = 2,                        # the initial consumer density
#'   r = 1,                                 # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   f_max = 6,                           # the maximum feeding rate
#'   n_half = 7500,                         # the half saturation density
#'   m = 2,                              # the mortality rate
#'   return_all_steps = TRUE                # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out2$t,
#'   log10(out2$n1),
#'   col = "green"
#' )
#' lines(
#'   out2$t,
#'   log10(out2$n2),
#'   col = "blue"
#' )
#'
#'
#' # a non-disturbed time series would look like:
#' out3 <- simulate_rma_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = rep(0, 21),        # the fraction of media replaced (or the strength of disturbance)
#'   n1_initial = 100,                      # the initial resource density
#'   n2_initial = 2,                        # the initial consumer density
#'   r = 1,                                 # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   f_max = 6,                           # the maximum feeding rate
#'   n_half = 7500,                         # the half saturation density
#'   m = 2,                              # the mortality rate
#'   return_all_steps = TRUE                # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out3$t,
#'   log10(out3$n1),
#'   col = "darkgreen",
#'   lty = 2,
#'   lwd = 2
#' )
#' lines(
#'   out3$t,
#'   log10(out3$n2),
#'   col = "darkblue",
#'   lty = 2,
#'   lwd = 2
#' )
#'
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
#' out <- simulate_rma_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n1_initial = 100,                      # the initial resource density
#'   n2_initial = 2,                        # the initial consumer density
#'   r = 1,                                 # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   f_max = 6,                           # the maximum feeding rate
#'   n_half = 7500,                         # the half saturation density
#'   m = 2                               # the mortality rate
#' )
#'
#' # plot the time series
#' par(las=1)
#' plot(
#'   c(out$t,out$t),
#'   log10(c(out$n1,out$n2)),
#'   type = "n",
#'   pch = 16,
#'   xlab = "time",
#'   ylab = "log10-density"
#' )
#' points(
#'   out$t,
#'   log10(out$n1),
#'   col = "green",
#'   pch = 16
#' )
#' points(
#'   out$t,
#'   log10(out$n2),
#'   col = "blue",
#'   pch = 16
#' )
#'
#' out2 <- simulate_rma_pulsed(
#'   t_pulse = df_in[,1],                   # time at which media is exchanged or disturbance happens
#'   fraction_exchanged = df_in[,2],        # the fraction of media replaced (or the strength of disturbance)
#'   n1_initial = 100,                      # the initial resource density
#'   n2_initial = 2,                        # the initial consumer density
#'   r = 1,                                 # the intrinsic growth rate
#'   K = 10000,                             # the carrying capacity
#'   f_max = 6,                           # the maximum feeding rate
#'   n_half = 7500,                         # the half saturation density
#'   m = 2,                              # the mortality rate
#'   return_all_steps = TRUE                # all time steps will be returned
#' )
#'
#' # add the in-between steps
#' lines(
#'   out2$t,
#'   log10(out2$n1),
#'   col = "green"
#' )
#' lines(
#'   out2$t,
#'   log10(out2$n2),
#'   col = "blue"
#' )
#'

simulate_rma_pulsed <- function(
    t_pulse,
    fraction_exchanged,
    n1_initial,
    n2_initial,
    r,
    K,
    f_max,
    n_half,
    m,
    t_length = 100,
    return_all_steps = FALSE
){

  i <- NULL

  t_out <- c(t_pulse[1])
  n1_out <- c(n1_initial)
  n2_out <- c(n2_initial)
  t_out_all <- c()
  n1_out_all <- c()
  n2_out_all <- c()

  # for loop between t_pulse steps


  for(i in 1:length(t_pulse[-1])){
    ##
    rma_model_assigned <- dynasymfitr:::rma_model$new(
      n1_initial = n1_out[i]*(1-fraction_exchanged[i]),
      n2_initial = n2_out[i]*(1-fraction_exchanged[i]),
      r = r,
      K = K,
      f_max = f_max,
      n_half = n_half,
      m = m
    )
    ##
    tt <- seq(
      t_out[i],
      t_pulse[i+1],
      length.out = t_length
    )
    ##
    pop_dens <- rma_model_assigned$run(tt)
    ##
    t_out <- c(t_out, pop_dens[nrow(pop_dens),1])
    n1_out <- c(n1_out, pop_dens[nrow(pop_dens),2])
    n2_out <- c(n2_out, pop_dens[nrow(pop_dens),3])
    t_out_all <- c(t_out_all, pop_dens[,1])
    n1_out_all <- c(n1_out_all, pop_dens[,2])
    n2_out_all <- c(n2_out_all, pop_dens[,3])
  }


  rm(i)

  # return the data frame

  if(return_all_steps){
    return_frame <- data.frame(
      t = t_out_all,
      n1 = n1_out_all,
      n2 = n2_out_all
    )
  } else{
    return_frame <- data.frame(
      t = t_out,
      n1 = n1_out,
      n2 = n2_out
    )
  }

  return(return_frame)

}
