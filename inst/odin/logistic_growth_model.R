deriv(n) <- r * n * (1 - n / K)                                                 # the ODE of the logistic growth model
initial(n) <- n_initial                                                         # assign initial resource density

# all parameters must be filled with a value, "user()" means that this
# information can be added later and is not hard coded
n_initial <- user()                                                             # initial resource density
r <- user()                                                                     # intrinsic growth rate
K <- user()                                                                     # carrying capacity
