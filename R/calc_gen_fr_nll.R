#' Calculate the negative log-likelihood of the generalized functional response model
#'
#' @description `calc_gen_fr_nll` calculates the negative log likelihood of the
#'     generalized functional response model (Real 1977, 1979), but see the
#'     description of `compile_gen_fr` for further information. The calculation
#'     of the likelihood assumes a binomial distribution of the dependent data,
#'     as every resource item has a chance to be eaten or not to be eaten
#'     throughout the experimental trial. For further details on the
#'     methodology, please read chapter eight of “Ecological models and data in
#'     R” (Bolker 2008). To restrict the fitting results to reasonable values of
#'     the shape parameter q (Williams and Martinez 2004, Vucic-Pestic et al.
#'     2010, Rosenbaum and Rall 2018) the negative log-likelihood calculation
#'     has following penalty function:
#'
#'     ```
#'     if(q < q_low){
#'       nll <- nll + penalty*(q-q_low)^2
#'     } else{
#'       if(q >= q_up){
#'         nll <- nll + penalty*(q-q_up)^2
#'       } else{
#'         nll <- nll
#'       }
#'     }
#'     ```
#'
#'     `q_low` is set to “0” by default (a type II functional response), `q_up`
#'     is set to “1” by default (a “strict” type III functional response), and
#'     `penalty` is set to 1000 by default. Especially `q_low` is important, as
#'     negative values may lead to an unsolvable time series for
#'     `simulate_gen_fr` leading to a crash of the fitting process. Even with
#'     this restriction, the simulation may fail for extreme values of F_max or
#'     N_half; in this case, the function returns `Inf`. Alternative solutions
#'     would be that the function returns `NA` (Bolker 2008). Moreover, the
#'     function requires the model parameters F_max and N_half on log-scale, as
#'     this transformation (1) accelerates the fitting procedure and (2)
#'     prevents biologically irrelevant negative estimations that would also
#'     crash the fitting algorithm.
#'
#' @references Bolker (2008) Ecological models and data in R, Princeton
#'     University Press, Princeton, New Jersey.
#'     https://math.mcmaster.ca/~bolker/emdbook/index.html
#' @references FitzJohn and Jombart (2024) odin: ODE generation and integration.
#'     Ver. 1.2.6. https://doi.org/10.32614/CRAN.package.odin
#'     see also: https://github.com/mrc-ide/odin
#' @references Microsoft and Weston (2022) foreach: provides foreach looping
#'     construct. Version 1.5.2. https://doi.org/10.32614/CRAN.package.foreach
#' @references Real (1977) The kinetics of functional response. Am Nat 111, 289-
#'     300. https://doi.org/10.1086/283161
#' @references Real (1979) Ecological determinants of functional response.
#'     Ecology 60, 481-485. https://doi.org/10.2307/1936067
#' @references Rosenbaum and Rall (2018) Fitting functional responses: Direct
#'     parameter estimation by simulating differential equations. Methods Ecol
#'     Evol 9, 2076-2090. https://doi.org/10.1111/2041-210X.13039
#' @references Vucic-Pestic et al. (2010) Allometric functional response model:
#'     body masses constrain interaction strengths. J Anim Ecol 79, 249-256.
#'     https://doi.org/10.1111/j.1365-2656.2009.01622.x
#' @references Williams and Martinez (2004) Stabilization of chaotic and
#'     non-permanent food-web dynamics. Eur Phys J B 38, 297-303.
#'     https://doi.org10.1140/epjb/e2004-00122-1
#'
#' @useDynLib dynafit
#' @importFrom odin odin
#' @importFrom stats dbinom
#'
#' @param n_eaten integer; the prey items that were eaten throughout
#'     the experimental trial. A vector.
#' @param n_initial integer; the initial prey density. A vector of
#'     the same length as n_eaten.
#' @param p  integer; the predator density. A vector of
#'     the same length as n_eaten.
#' @param f_max_log10 float; the log10 maximum feeding rate.
#' @param n_half_log10 float; the log10 half saturation density.
#' @param q float; shape parameter, a single value. A strict type II functional
#'     has q = 0, a strict type III functional response has q = 1.
#' @param t_start integer or float; the time were the feeding starts. A single
#'     value; default = 0.
#' @param t_end integer or float; the time were the feeding ends. A single
#'     value; default = 1 (e.g. 1 day).
#' @param t_length integer or float; the number of time steps that should be
#'     generated. The more time steps, the more precise the simulation. A single
#'     value; default = 100.
#' @param penalty a penalty that is added to the nll if the value of q is below
#'     q_low or above q_up. The default= 1000. Equation:
#'         if(q < q_low) nll + penalty*(q-q_low)^2
#'         if(q > q_up) nll + penalty*(q-q_up)^2
#' @param q_low lower soft boundary of q, default = 0 (Type II FR).
#' @param q_up upper soft boundary of q, default = 1 (Type III FR).
#'
#' @return Returns a single negative log-likelihood value.
#'
#' @export
#'
#' @examples
#'
#' fr_data <- data_vucic_pestic_et_al_2010_j_anim_ecol
#'
#' fit <- bbmle::mle2(
#'   minuslogl = calc_gen_fr_nll,
#'   start = list(
#'     f_max_log10  = log10(max(fr_data$n_eaten)),
#'     n_half_log10 = log10(mean(fr_data$n_initial)),
#'     q = 0.2
#'   ),
#'   data = list(
#'     n_eaten = fr_data$n_eaten,
#'     n_initial = fr_data$n_initial,
#'     p = rep(1, nrow(fr_data)),
#'     t_end = rep(1, nrow(fr_data))
#'   ),
#'   control = list(reltol = 1e-12, maxit = 1000)
#' )
#'
#' bbmle::summary(fit)
#'

calc_gen_fr_nll <- function(
    n_eaten,
    n_initial,
    p,
    t_end,
    t_start = 0,
    t_length = 1000,
    f_max_log10,
    n_half_log10,
    q,
    penalty = 1000,
    q_low = 0,
    q_up = 1
){

  nll <- Inf

  try({
    eaten_simulated <- simulate_gen_fr(
      n_initial = n_initial,
      p = p,
      f_max = 10^f_max_log10,
      n_half = 10^n_half_log10,
      q = q,
      t_start = t_start,
      t_end = t_end,
      t_length = t_length
    )

    lls <- stats::dbinom(
      x = n_eaten,
      size = n_initial,
      prob = eaten_simulated$n_eaten/n_initial,
      log = T
    )

    nll <- -1*sum(lls)

  }, silent = TRUE)

  if(nll == Inf){return(nll)}

  # penalty if q < 0 or q > 1 (changeable in function header!)
  if(q < q_low){
    nll <- nll + penalty*(q-q_low)^2
  } else{
    if(q >= q_up){
      nll <- nll + penalty*(q-q_up)^2
    } else{
      nll <- nll
    }
  }

  return(nll)
}
