#' compile_gen_fr: compiles a generalized functional response ODE using odin
#'
#' @description `compile_gen_fr` compiles the ordinary differential equation
#'     (ODE) describing the decay of resource items over time during a feeding
#'     functional response trial. The functional response model has a free shape
#'     parameter (Real 1977, 1979), allowing a continuous shift of the shape of
#'     a classic type II functional response (Holling 1959b) to a type III
#'     functional response (Holling 1959a). The feeding rate, F, depends on the
#'     resource density, N, and the parameters of the model are the maximum
#'     feeding rate, F_max, the half saturation density, N_half, and the shape
#'     parameter q (Williams and Martinez 2004, Vucic-Pestic et al. 2010):
#'
#'     F = F_max * N^(1+q) / (N_half^(1+q) + N^(1+q)).
#'
#'     The resulting ODE, describing the decay of the resource over time, dN/dt,
#'     is:
#'
#'     dN/dt = -F_max * N^(1+q) / (N_half^(1+q) + N^(1+q)).
#'
#'     A numerical simulation is required as there is no analytical solution for
#'     this problem (Rosenbaum and Rall 2018). We use the R package “odin”
#'     (FitzJohn 2024) to create a fast simulation model in C. This model can be
#'     used to estimate the exact shape of the functional response or test if a
#'     functional response is type II or type III (Rosenbaum and Rall 2018).
#'
#' @references FitzJohn and Jombart (2024) odin: ODE generation and integration.
#'     Ver. 1.2.6. https://doi.org/10.32614/CRAN.package.odin
#'     see also: https://github.com/mrc-ide/odin
#' @references Holling (1959a) The components of predation as revealed by a
#'     study of small-mammal predation of the european pine sawfly. Can Entomol
#'     91, 293-320. https://doi.org/10.4039/Ent91293-5
#' @references Holling (1959b) Some characteristics of simple types of predation
#'     and parasitism. Can Entomol 91, 385-398.
#'     https://doi.org/10.4039/Ent91385-7
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
#' @import odin
#'
#' @return The function compiles the functional response model and returns a
#'     function called `gen_fr_model` that is needed to simulate the time series
#'     of decaying resource items.
#'
#' @export
#'
#' @examples
#'
#' # You may also want to see a broader introduction on how to use odin on their
#' # webpage: https://mrc-ide.github.io/odin/articles/odin.html
#'
#' # compiling the model in C using odin
#' compile_gen_fr()
#'
#' # assign parameter values
#' gen_fr_model_assigned <- gen_fr_model$new(
#'   n_initial = 5,     # initial prey number (or density)
#'   f_max = 18,        # maximum feeding rate
#'   n_half = 3,        # half saturation density
#'   q = 0,             # shape parameter (type II functional response)
#'   p = 1              # predator number (or density, fixed)
#' )
#'
#' # defining time steps to compute, the more the better but slower
#' tt <- seq(
#'   0,                 # starts at time zero
#'   1,                 # ends at time = 1, e.g. one day
#'   length.out = 1000  # computes 1000 steps
#' )
#'
#' # simulate the time series of decaying prey
#' out <- gen_fr_model_assigned$run(tt)
#'
#' # plot results
#' plot(
#'   out[,1],
#'   out[,2],
#'   type = "l"
#' )
#'

compile_gen_fr <- function(){
  gen_fr_model <<- odin::odin({
    deriv(n) <- -f_max * n^(1+q) / (n_half^(1+q) + n^(1+q)) * p                 # the ODE of the generalized functional response
    initial(n) <- n_initial                                                     # assign initial prey density

    # all parameters must be filled with a value, "user()" means that this
    # information can be added later and is not hard coded
    n_initial <- user()                                                         # initial resource density
    f_max <- user()                                                             # maximum feeding rate
    n_half <- user()                                                            # half saturation density
    q <- user()                                                                 # shape parameter
    p <- user()                                                                 # predator / consumer density
  })
}
