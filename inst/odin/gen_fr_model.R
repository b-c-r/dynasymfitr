deriv(n) <- -f_max * n^(1+q) / (n_half^(1+q) + n^(1+q)) * p                     # the ODE of the generalized functional response
initial(n) <- n_initial                                                         # assign initial prey density

# all parameters must be filled with a value, "user()" means that this
# information can be added later and is not hard coded
n_initial <- user()                                                             # initial resource density
f_max <- user()                                                                 # maximum feeding rate
n_half <- user()                                                                # half saturation density
q <- user()                                                                     # shape parameter
p <- user()                                                                     # predator / consumer density
