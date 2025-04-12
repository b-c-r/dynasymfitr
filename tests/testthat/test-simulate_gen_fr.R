test_that("simulate_gen_fr", {
  expect_equal(
    {
      round(simulate_gen_fr(
        n_initial = 100,    # vector of initial prey densities
        p = 1,              # fixed predator density
        t_end = 1,          # fixed end time
        f_max = 10,         # maximum feeding rate
        n_half = 25,        # half saturation density
        q = 1               # shape parameter (1 = s-shaped)
      )[,2],5)

    },
    9.35497
    )
})
