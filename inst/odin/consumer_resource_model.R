deriv(n1) <- r*n1*(1-n1/K) - fmax*n1^(1+q)/(nhalf^(1+q)+n1^(1+q))*n2            # the ODE of the logistic growth model minus consumer feeding
deriv(n2) <- e*fmax*n1^(1+q)/(nhalf^(1+q)+n1^(1+q))*n2 - m*n2                   # consumer feeding/growth minus consumer loss
initial(n1) <- n1_initial                                                       # assign initial resource density
initial(n2) <- n2_initial                                                       # assign initial consumer density

# all parameters must be filled with a value, "user()" means that this
# information can be added later and is not hard coded
n1_initial <- user()                                                            # initial resource density
n2_initial <- user()                                                            # initial consumer density
r <- user()                                                                     # intrinsic growth rate
K <- user()                                                                     # carrying capacity
e <- user()                                                                     # trophic level transfer efficiency
fmax <- user()                                                                  # maximum feeding rate
nhalf <- user()                                                                 # half saturation density
q <- user()                                                                     # functional response shape parameter
m <- user()                                                                     # mortality or metabolic loss rate
