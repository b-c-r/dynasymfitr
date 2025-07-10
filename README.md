dynasymfitr: Fitting dynamic simulated ecological population models to
laboratory data
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

<!-- badges: start -->

<!-- badges: end -->

The goal of `dynasymfitr` is to provide a set of functions to analyze
laboratory species interaction experiments in a way that the parameter
estimates can be seamlessly integrated into theoretical/computational
eco-evo projects. `dynasymfitr` will be developed within the DFG-funded
research unit 5726, [DynaSym](https://www.dynasym.uni-konstanz.de/), but
is not limited to it.

Estimating density-dependent species interactions is not trivial. The
possibly most prominent example is the density-dependent feeding rate,
also known as functional response (Holling 1959). Feeding is a temporal
process, which requires that the functional response model be modeled as
a differential equation describing the decay of the resource over time
(Rosenbaum and Rall 2018). While a so-called analytical solution exists
for the classical hyperbolic type II functional response (Royama 1971;
Rogers 1972; Bolker 2008), the differential equations of other
functional response types need to be solved numerically while being
fitted to data (Rosenbaum and Rall 2018) to avoid biases in the
parameter evaluation. In addition to the above-mentioned problem, if
using small organisms such as algae as resources and ciliates as
consumers, even in short-term experiments these critters may grow or
decline in population size (i.e., the numerical response) due to death
and reproduction. This issue would add even more bias to the estimated
parameters. However, by simulating the dynamics of resources and
consumers explicitly, these biases can be corrected (Rosenbaum and Rall
2018). [DynaSym](https://www.dynasym.uni-konstanz.de/) researchers will
predominantly work in aquatic planktonic environments to detect the
strength and sign of symbiotic species interactions (Becks, Gaedke, and
Klauschies 2025). Therefore, we will not only add models describing
species interactions as mentioned above, but also widely used nutrient
uptake models (Tilman 1977; Huisman and Weissing 1999; Brose 2008; Zhang
and Becks 2024).

Please note that this package is under development, and we will also add
more information to the README in the future.

## Installation

You can install the development version of dynasymfitr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("b-c-r/dynasymfitr")
```

## Examples

### Modelling resource decay over time

First, load the `dynasymfitr` and the `bbmle` package:

``` r
library("dynasymfitr")
# install.packages("bbmle")
library("bbmle")
```

Transfer the data to an object with a shorter name:

``` r
fr_data <- data_vucic_pestic_et_al_2010_j_anim_ecol
```

Fit the model to the data:

``` r
fit <- bbmle::mle2(
  minuslogl = calc_gen_fr_nll,
  start = list(
    f_max_log10  = log10(max(fr_data$n_eaten)),
    n_half_log10 = log10(mean(fr_data$n_initial)),
    q = 0.2
  ),
  
  data = list(
    n_eaten = fr_data$n_eaten,
    n_initial = fr_data$n_initial,
    p = rep(1, nrow(fr_data)),
    t_end = rep(1, nrow(fr_data))
  ),
  control = list(reltol = 1e-12, maxit = 1000)
)
```

See the summary table:

``` r
bbmle::summary(fit)
#> Maximum likelihood estimation
#> 
#> Call:
#> bbmle::mle2(minuslogl = calc_gen_fr_nll, start = list(f_max_log10 = log10(max(fr_data$n_eaten)), 
#>     n_half_log10 = log10(mean(fr_data$n_initial)), q = 0.2), 
#>     data = list(n_eaten = fr_data$n_eaten, n_initial = fr_data$n_initial, 
#>         p = rep(1, nrow(fr_data)), t_end = rep(1, nrow(fr_data))), 
#>     control = list(reltol = 1e-12, maxit = 1000))
#> 
#> Coefficients:
#>              Estimate Std. Error z value     Pr(z)    
#> f_max_log10  1.399850   0.038288 36.5607 < 2.2e-16 ***
#> n_half_log10 1.471545   0.083398 17.6449 < 2.2e-16 ***
#> q            0.597590   0.196908  3.0349  0.002406 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> -2 log L: 307.7785
```

Simulate the best fit:

``` r
out <- simulate_gen_fr(
 n_initial = 0:1000,                 # vector of initial prey densities
 p = rep(1,1001),                    # fixed predator density
 t_end = rep(1,1001),                # fixed end time of the experiment
 f_max = 10^bbmle::coef(fit)[[1]],   # maximum feeding rate
 n_half = 10^bbmle::coef(fit)[[2]],  # half saturation density
 q = bbmle::coef(fit)[[3]]           # shape parameter (1 = s-shaped)
)
```

Create the regression plot:

``` r
# plot results
plot(
 fr_data$n_initial,
 fr_data$n_eaten,
 ylab = "number of prey eaten",
 xlab = "initial prey density"
)
lines(
 out[,1],
 out[,2]
)
```

<img src="man/figures/README-fr_example-1.png" width="100%" />

## Funding Information

- Björn C. Rall gratefully acknowledges the funding by the [**German
  Science Foundation (DFG) to the Research Unit DynaSym (FOR
  5726)**](https://gepris.dfg.de/gepris/projekt/528028597).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-BecksEtAl2025EmergentFeedbackSymbiosis" class="csl-entry">

Becks, Lutz, Ursula Gaedke, and Toni Klauschies. 2025. “Emergent
Feedback Between Symbiosis Form and Population Dynamics.” *Trends in
Ecology & Evolution* 0 (0): 0.
<https://doi.org/10.1016/j.tree.2025.02.006>.

</div>

<div id="ref-Bolker2008EcologicalModelsData" class="csl-entry">

Bolker, Benjamin M. 2008. *Ecological Models and Data in R*. Princeton,
New Jersey: Princeton University Press.

</div>

<div id="ref-Brose2008ComplexFoodWebs" class="csl-entry">

Brose, Ulrich. 2008. “Complex Food Webs Prevent Competitive Exclusion
Among Producer Species.” *Proceedings of the Royal Society B: Biological
Sciences* 275 (1650): 2507–14. <https://doi.org/10.1098/rspb.2008.0718>.

</div>

<div id="ref-Holling1959CharacteristicsSimpleTypes" class="csl-entry">

Holling, C. S. 1959. “Some Characteristics of Simple Types of Predation
and Parasitism.” *The Canadian Entomologist* 91 (7): 385–98.
<https://doi.org/10.4039/Ent91385-7>.

</div>

<div id="ref-HuismanWeissing1999BiodiversityPlanktonSpecies"
class="csl-entry">

Huisman, Jef, and Franz J. Weissing. 1999. “Biodiversity of Plankton by
Species Oscillations and Chaos.” *Nature* 402 (6760): 407–10.
<https://doi.org/10.1038/46540>.

</div>

<div id="ref-Rogers1972RandomSearchInsect" class="csl-entry">

Rogers, David. 1972. “Random Search and Insect Population Models.” *The
Journal of Animal Ecology* 41 (2): 369–83.
<https://doi.org/10.2307/3474>.

</div>

<div id="ref-RosenbaumRall2018FittingFunctionalResponses"
class="csl-entry">

Rosenbaum, Benjamin, and Björn Christian Rall. 2018. “Fitting Functional
Responses: Direct Parameter Estimation by Simulating Differential
Equations.” *Methods in Ecology and Evolution* 9 (10): 2076–90.
<https://doi.org/10.1111/2041-210X.13039>.

</div>

<div id="ref-Royama1971ComparativeStudyModels" class="csl-entry">

Royama, T. 1971. “A Comparative Study of Models for Predation and
Parasitism.” *Researches on Population Ecology* 13 (1): 1–91.
<https://doi.org/10.1007/BF02511547>.

</div>

<div id="ref-Tilman1977ResourceCompetitionPlankton" class="csl-entry">

Tilman, David. 1977. “Resource Competition Between Plankton Algae: An
Experimental and Theoretical Approach.” *Ecology* 58 (2): 338–48.
<https://doi.org/10.2307/1935608>.

</div>

<div id="ref-ZhangBecks2024MechanisticRulesSpecies" class="csl-entry">

Zhang, Zhijie, and Lutz Becks. 2024. “The Mechanistic Rules for Species
Coexistence.” bioRxiv. <https://doi.org/10.1101/2024.05.07.592948>.

</div>

</div>
