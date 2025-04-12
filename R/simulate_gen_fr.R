#' simulate_gen_fr: simulates a generalized functional response time series
#'
#' @description `simulate_gen_fr` simulates time series across different initial
#'     resource densities using `odin` (FitzJohn and Jombart 2024). It returns
#'     the number of initial prey items and the number of prey eaten at the end
#'     of the time series, mimicking common functional response laboratory
#'     experiments. The underlying functional response model is the generalized
#'     functional response model (Real 1977, 1979). Note that not integers, but
#'     floating numbers are returned by `simulate_gen_fr`.
#'
#' @references FitzJohn and Jombart (2024) odin: ODE generation and integration.
#'     Ver. 1.2.6. https://doi.org/10.32614/CRAN.package.odin
#'     see also: https://github.com/mrc-ide/odin
#' @references Real (1977) The kinetics of functional response. Am Nat 111, 289-
#'     300. https://doi.org/10.1086/283161
#' @references Real (1979) Ecological determinants of functional response.
#'     Ecology 60, 481-485. https://doi.org/10.2307/1936067
#'
#' @seealso [compile_gen_fr()]
#'
#' @include compile_gen_fr.R
#'
#' @import foreach
#'
#' @param n_initial integer or float; a vector of initial prey densities.
#' @param p integer or float; a single value of a fixed predator density. The
#'     default value is 1.
#' @param f_max float; maximum feeding rate, a single value.
#' @param n_half float; half saturation density, a single value.
#' @param q float; shape parameter, a single value. A strict type II functional
#'     has q = 0, a strict type III functional response has q = 1.
#' @param t_start integer or float; the time were the feeding starts. A single
#'     value; default = 0.
#' @param t_end integer or float; the time were the feeding ends. A single
#'     value; default = 1 (e.g. 1 day).
#' @param t_length integer or float; the number of time steps that should be
#'     generated. The more time steps, the more precise the simulation. A single
#'     value; default = 1000.
#'
#' @return Returns a data frame with n_initial and the according n_eaten.
#'
#' @export
#'
#' @examples
#'
#' # simulate the functional response:
#' out <- simulate_gen_fr(
#'  n_initial = 1:100,    # vector of initial prey densities
#'  p = 1,                # fixed predator density
#'  f_max = 10,           # maximum feeding rate
#'  n_half = 25,          # half saturation density
#'  q = 1                 # shape parameter (1 = s-shaped)
#' )
#'
#' # plot the functional response
#' plot(
#'   out$n_initial,
#'   out$n_eaten,
#'   type = "l"
#' )
#'

simulate_gen_fr <- function(
    n_initial,
    p = 1,
    f_max,
    n_half,
    q,
    t_start = 0,
    t_end = 1,
    t_length = 1000
){

  # compile the ode solver
  compile_gen_fr()

  # foreach loop across all initial prey densities:
  n_eaten <- foreach::foreach(
    i = 1:length(n_initial),
    .combine = c
  ) %do% {

    # assign values to model parameters
    gen_fr_model_assigned <- gen_fr_model$new(
      n_initial = n_initial[i],
      f_max = f_max,
      n_half = n_half,
      q = q,
      p = p
    )

    # set time steps to be computed
    tt <- seq(
      t_start,
      t_end,
      length.out = t_length
    )

    # calculate the remaining prey items/density
    remaining_prey <- gen_fr_model_assigned$run(tt)[[t_length,2]]

    # calculate the number of prey eaten (start - final prey density):
    n_initial[i] - remaining_prey
  }

  # return the data frame
  data.frame(
    n_initial = n_initial,
    n_eaten = n_eaten
  )
}
