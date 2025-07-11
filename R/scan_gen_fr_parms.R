#' scan_gen_fr_parms: scans for reasonable starting parameters before fitting
#'
#' @description `scan_gen_fr_parms` creates Latin hypercube samples for the
#'     functional response parameters in a reasonable range and calculates the
#'     according negative log-likelihood values. It returns the parameter values
#'     with the lowest negative log likelihood of these samples. Non-linear
#'     maximum likelihood fitting procedures require starting parameters,
#'     generally based on an educated guess (e.g., Bolker 2008). Moreover,
#'     these fits may end up in local best fits, and users should re-fit the
#'     data using different starting parameters (Bolker 2008). To overcome
#'     manually eyeballing as well as re-shuffling the starting parameters,
#'     Jager and Ashauer (2018) suggested creating samples in a reasonable
#'     parameter range using and choosing the starting parameters (from the
#'     lowest nll value) from these samples. To reduce the number of required
#'     samples by keeping the variance of parameter values as wide as possible,
#'     it is recommended to use Latin hypercube sampling. `scan_gen_fr_parms`
#'     requires the lhs package (Carnell 2024). See also the description of
#'     `calc_gen_fr_nll` for further information.
#'
#' @references Bolker (2008) Ecological models and data in R, Princeton
#'     University Press, Princeton, New Jersey.
#'     https://math.mcmaster.ca/~bolker/emdbook/index.html
#' @references Carnell (2024) lhs: latin hypercube samples. Version 1.2.0.
#'     https://CRAN.R-project.org/package=lhs
#' @references Jager and Ashauer (2018) Modelling survival under chemical stress
#'     Leanpub. https://leanpub.com/guts_book
#'
#' @useDynLib dynasymfitr
#'
#' @include simulate_gen_fr.R
#' @include calc_gen_fr_nll.R
#'
#' @import foreach
#' @importFrom odin odin
#' @import lhs
#' @import parallel
#' @import doParallel
#'
#' @param n_eaten integer (or float); the prey items that were eaten throughout
#'     the experimental trial. A vector.
#' @param n_initial integer (or float); the initial prey density. A vector of
#'     the same length as n_eaten.
#' @param p The predator density. A single value
#' @param f_max_range_log10 float; a range (2 values) of the log10 of the
#'     maximum feeding rate.
#' @param n_half_range_log10 float; a range (2 values) of the log10 of the half
#'     saturation density.
#' @param q_range float; shape parameter, a range (2 values). A strict type II
#'     functional has q = 0, a strict type III functional response has q = 1.
#'     The values should match the values q_low and q_up below.
#'     Default is c(0,1).
#' @param t_start integer or float; the time were the feeding starts. A single
#'     value; default = 0.
#' @param t_end integer or float; the time were the feeding ends. A single
#'     value; default = 1 (e.g. 1 day).
#' @param t_length integer or float; the number of time steps that should be
#'     generated. The more time steps, the more precise the simulation. A single
#'     value; default = 1000.
#' @param penalty a penalty that is added to the nll if the value of q is below
#'     q_low or above q_up. The default= 1000. Equation:
#'         if(q < q_low) nll + penalty*(q-q_low)^2
#'         if(q > q_up) nll + penalty*(q-q_up)^2
#' @param q_low lower soft boundary of q, default = 0 (Type II FR).
#' @param q_up upper soft boundary of q, default = 1 (Type III FR).
#' @param no_lhs_samples a single integer value; the number of random latin
#'     hypercube samplings. Default = 1000.
#' @param use_parallel_computing Unleash the power of your computer by allow for
#'     parallel computing. Default is FALSE.
#' @param max_no_threads Only used if `use_parallel_computing` is TRUE. The
#'     number of threads that should be used for parallel computing. Don't use
#'     too many cores and do not overload your system. Default = 1.
#' @param force_all_threads Would you like to force the function to use the
#'     maximum number of threads you defined in `max_no_threads`. If FALSE, the
#'     functions tries to find a sweet spot of threads that depend on
#'     `no_lhs_samples`. E.g., if you define to use 16 threads, and 1000 samples
#'     should be taken it only uses 10 to get an even use of threads.
#' @param return_all_trials Should all estimates be returned? Default is FALSE.
#'     In this case only the parameters for the lowest nll are returned.
#'
#' @return Returns a data frame with a single row of parameter values. If
#'     `return_all_trials` is TRUE all estimates are returned.
#'
#' @export
#'
#' @examples
#'
#'
#' fr_data <- data_vucic_pestic_et_al_2010_j_anim_ecol
#'
#' # Compute the results sequentially:
#'
#' time_seq <- system.time({
#'   results_seq <- scan_gen_fr_parms(
#'     n_eaten = fr_data$n_eaten,
#'     n_initial = fr_data$n_initial,
#'     p = rep(1, nrow(fr_data)),
#'     t_end = rep(1, nrow(fr_data)),
#'     f_max_range_log10 = log10(c(1, max(fr_data$n_eaten))),
#'     n_half_range_log10 = log10(c(1, max(fr_data$n_initial))),
#'     q_range = c(0, 1),
#'     no_lhs_samples = 100
#'   )
#' })[[3]]
#'
#' # the results:
#' results_seq
#'
#'
#' time_par <- system.time({
#'   results_par <- scan_gen_fr_parms(
#'     n_eaten = fr_data$n_eaten,
#'     n_initial = fr_data$n_initial,
#'     p = rep(1, nrow(fr_data)),
#'     t_end = rep(1, nrow(fr_data)),
#'     f_max_range_log10 = log10(c(1, max(fr_data$n_eaten))),
#'     n_half_range_log10 = log10(c(1, max(fr_data$n_initial))),
#'     q_range = c(0, 1),
#'     no_lhs_samples = 100,
#'     use_parallel_computing = TRUE,
#'     max_no_threads = 2
#'   )
#' })[[3]]
#'
#' # the results:
#' results_par
#'
#' # compare the time required:
#' time_seq
#' time_par
#'

scan_gen_fr_parms <- function(
    n_eaten,
    n_initial,
    p,
    t_end,
    t_start = 0,
    t_length = 1000,
    f_max_range_log10,
    n_half_range_log10,
    q_range,
    penalty = 1000,
    q_low = 0,
    q_up = 1,
    no_lhs_samples = 1000,
    use_parallel_computing = FALSE,
    max_no_threads = 1,
    force_all_threads = FALSE,
    return_all_trials = FALSE
){

  lhsvals <- lhs::randomLHS(no_lhs_samples, 3)

  f_max_range_log10  <- log10((lhsvals[,1] * (10^f_max_range_log10[2]  - 10^f_max_range_log10[1] )) + 10^f_max_range_log10[1])
  n_half_range_log10 <- log10((lhsvals[,2] * (10^n_half_range_log10[2] - 10^n_half_range_log10[1])) + 10^n_half_range_log10[1])
  q_range            <- (lhsvals[,3] * (q_range[2]-q_range[1])) + q_range[1]


  i <- NULL

  if(!use_parallel_computing){
    ## use one thread:

    ## calculate nlls
    nlls <- foreach::foreach(
      i = 1:no_lhs_samples,
      .combine = "c") %do% {

        nll <- calc_gen_fr_nll(
          n_eaten = n_eaten,
          n_initial = n_initial,
          p = p,
          t_end = t_end,
          t_start = t_start,
          t_length = t_length,
          f_max_log10 = f_max_range_log10[i],
          n_half_log10 = n_half_range_log10[i],
          q = q_range[i],
          penalty = penalty,
          q_low = q_low,
          q_up = q_up
        )

        return(nll)
      }

  } else{
    if(max_no_threads == 1){

      ## use one thread:
      ## calculate nlls
      nlls <- foreach::foreach(
        i = 1:no_lhs_samples,
        .combine = "c") %do% {

          nll <- calc_gen_fr_nll(
            n_eaten = n_eaten,
            n_initial = n_initial,
            p = p,
            t_end = t_end,
            t_start = t_start,
            t_length = t_length,
            f_max_log10 = f_max_range_log10[i],
            n_half_log10 = n_half_range_log10[i],
            q = q_range[i],
            penalty = penalty,
            q_low = q_low,
            q_up = q_up
          )

          return(nll)
        }

    } else{

      ## use more than one thread:

      if(!force_all_threads){
        no_threads <- max((1:max_no_threads)[no_lhs_samples %% 1:max_no_threads == 0])
      } else{
        no_threads <- max_no_threads
      }

      cl <- parallel::makeCluster(no_threads)
      doParallel::registerDoParallel(cl)

      ## calculate nlls
      nlls <- foreach::foreach(
        i = 1:no_lhs_samples,
        .packages = c("foreach", "dynasymfitr"),
        .combine = "c") %dopar% {

          nll <- calc_gen_fr_nll(
            n_eaten = n_eaten,
            n_initial = n_initial,
            p = p,
            t_end = t_end,
            t_start = t_start,
            t_length = t_length,
            f_max_log10 = f_max_range_log10[i],
            n_half_log10 = n_half_range_log10[i],
            q = q_range[i],
            penalty = penalty,
            q_low = q_low,
            q_up = q_up
          )

          return(nll)
        }
        parallel::stopCluster(cl)

    }
  }

  rm(i)

  if(!return_all_trials){
    sel_parms <- data.frame(
      f_max_log10 = f_max_range_log10[nlls == min(nlls)],
      n_half_log10 = n_half_range_log10[nlls == min(nlls)],
      q = q_range[nlls == min(nlls)],
      nll = nlls[nlls == min(nlls)]
    )
  } else{
    sel_parms <- data.frame(
      f_max_log10 = f_max_range_log10,
      n_half_log10 = n_half_range_log10,
      q = q_range,
      nll = nlls
    )
  }

  return(sel_parms)
}
