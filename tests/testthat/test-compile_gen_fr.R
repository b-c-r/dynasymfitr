test_that("multiplication works", {
  expect_equal(
    {
      compile_gen_fr()
      gen_fr_model_assigned <- gen_fr_model$new(
        n_initial = 5,                                                          # initial prey number (or density)
        f_max = 18,                                                             # maximum feeding rate
        n_half = 3,                                                             # half saturation density
        q = 0,                                                                  # shape parameter (q = 0 is a type II functional response)
        p = 1                                                                   # predator number (or density, fixed)
      )

      # defining time steps to compute, the more the better but slower
      tt <- seq(
        0,                                                                      # starts at time zero
        1,                                                                      # ends at time = 1, e.g. one day
        length.out = 1000                                                       # computes 1000 steps
      )

      # simulate the time series of decaying prey
      gen_fr_model_assigned$run(tt)[,2][1000]

    }, 0.064228709)
})
