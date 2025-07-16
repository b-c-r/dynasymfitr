deriv(n1) <- r*n1*(1-n1/K) - f_max*n1/(n_half+n1)*n2                            # the ODE of the logistic growth model minus consumer feeding
deriv(n2) <- f_max*n1/(n_half+n1)*n2 - m*n2                                     # consumer feeding/growth minus consumer loss
initial(n1) <- n1_initial                                                       # assign initial resource density
initial(n2) <- n2_initial                                                       # assign initial consumer density

# all parameters must be filled with a value, "user()" means that this
# information can be added later and is not hard coded
n1_initial <- user()                                                            # initial resource density
n2_initial <- user()                                                            # initial consumer density
r <- user()                                                                     # intrinsic growth rate
K <- user()                                                                     # carrying capacity
f_max <- user()                                                                 # maximum feeding rate
n_half <- user()                                                                # half saturation density
m <- user()                                                                     # mortality or metabolic loss rate
