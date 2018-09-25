#Baseline no-synergy/no-antagonism MIXDER (mixed field dose-effect relation(s)) based on any input
MIXDER_function = function(r, L, Z.beta, d = seq(0, 0.2, by = 0.001), eta0 = 2.487899e-04, eta1 = 8.994540e-03, sig0 = 5.956631e+00, kap = 6.950537e+02) {
  dE=function(yini,State,Pars){
    eta0 = eta0; eta1 = eta1; sig0 = sig0; kap = kap
    with(as.list(c(State, Pars)), {
      P = vector(length = length(L))
      sig = vector(length = length(L))
      etaa = vector(length = length(L))
      u = vector(length = length(L))
      for (i in 1:length(L)) {
        P[i] = (1-exp(-Z.beta[i]/kap))^2
        sig[i] = sig0*P[i] + 0.041/6.24*L[i]*(1-P[i])
        etaa[i] = eta0*L[i]*exp(-eta1*L[i])
        u[i] = uniroot(function(d) sig[i]*6.24*d/L[i]*(1-exp(-1024*d/L[i])) + etaa[i]*(1-exp(-10^5*d)) - I, lower = 0, upper = 1, extendInt = "yes", tol = 10^-10)$root 
      }
      dI = vector(length = length(L))
      for (i in 1:length(L)) {
        dI[i] = r[i]*(sig[i]*6.24/L[i]*exp(-1024*u[i]/L[i])*(exp(1024*u[i]/L[i]) + 1024*u[i]/L[i] - 1) + etaa[i]*10^5*exp(-10^5*u[i])) 
      }
      dI = sum(dI)
      return(list(c(dI)))
    })
  }
  pars = NULL; yini = c(I= 0); d = d
  out = ode(yini,times = d, dE, pars, method = "radau")
  return(out)
}

#Confidence intervals (CI) and Monte Carlo (MC) simulations 
#MonteCarlo using multivariate gaussian 
require(mvtnorm)
sig = vcov(IDER_model)
set.seed(10)
monte_carlo_parameters = rmvnorm(n = 500, mean = c(eta0 = 2.487899e-04, eta1 = 8.994540e-03, sig0 = 5.956631e+00, kap = 6.950537e+02), sigma = sig)
eta0_MC = monte_carlo_parameters[, 1]
eta1_MC = monte_carlo_parameters[, 2]
sig0_MC = monte_carlo_parameters[, 3]
kap_MC = monte_carlo_parameters[, 4]
kap_MC[kap_MC <= 1e-6] = 1e-5 
#note because our last parameter were less significant when inputting our variance covariance matrix the parameters had to actually be readjusted to fix for negative value. This is important because if kap parameter went negative our model is nonsensical. (Luckily the fix was only for exactly 15 out of 500 MC samples or only roughly 3%)

#This is a general CI_function where you just get a CI for a specific dose point for any synergy analysis using the monte carlo simulations using vcov().
CI_function_MIXDER = function(d, d_interested, r, interval = 0.95, L, Z.beta) {
  MIXDER_curve = list(0)
  for (i in 1:500) {
    MIXDER_curve[[i]] = MIXDER_function(r = r, d = d, L = L, Z.beta = Z.beta, eta0 = eta0_MC[i], eta1 = eta1_MC[i], sig0 = sig0_MC[i], kap = kap_MC[i])
  }
  info = vector(length = 0)
  for (i in 1:500) {
    info = c(info, MIXDER_curve[[i]][, 2][2])
  }
  info = sort(info)
  lower_bound = info[(1-interval)/2*500]
  upper_bound = info[(interval + (1-interval)/2)*500]
  CI = c(lower_bound, upper_bound)
  return(CI)
}

#Monte Carlo using the error bars from summary()
set.seed(11)
eta0_MC_2 = rnorm(1000, mean = 1.485e-04, sd = 2.190e-05)
eta1_MC_2 = rnorm(1000, mean = 3.514e-03, sd = 9.037e-04)
sig0_MC_2 = rnorm(1000, mean = 4.150e+00, sd = 1.366e+00)
kap_MC_2 = rnorm(1000, mean = 4.688e+02, sd = 2.469e+02)
kap_MC_2[kap_MC_2 <= 1e-6] = 1e-5 

CI_function_MIXDER_MC_2 = function(d, d_interested, r, interval = 0.95, L, Z.beta) {
  MIXDER_curve = list(0)
  for (i in 1:500) {
    MIXDER_curve[[i]] = MIXDER_function(r = r, d = d, L = L, Z.beta = Z.beta, eta0 = eta0_MC_2[i], eta1 = eta1_MC_2[i], sig0 = sig0_MC_2[i], kap = kap_MC_2[i])
  }
  info = vector(length = 0)
  for (i in 1:500) {
    info = c(info, MIXDER_curve[[i]][, 2][2])
  }
  info = sort(info)
  lower_bound = info[(1-interval)/2*500]
  upper_bound = info[(interval + (1-interval)/2)*500]
  CI = c(lower_bound, upper_bound)
  return(CI)
}
#6 ions analysis using 95% CI ribbons using both methods
#This is for the six_ion synergy analysis incorporating our monte carlo simulations with the yellow ribbon as the 95% confidence interval using our monte carlo simulated parameters with two panels for two methods of the MC simulation. Similarly, blues curves are individual IDERs and red curve the mixed IDER and the black the simple effect addtivity curve.

#First the MIXDER of the true curve 
out = MIXDER_function(r = rep(1/6, times = 6), L = c(75, 100, 125, 175, 195, 240), Z.beta = c(595, 690, 770, 1075, 1245, 1585), d = c(seq(0, 0.01, 0.001), seq(0.01, 0.4, by = 0.01)))
six_ion_MIXDER = data.frame(d = out[, 1], CA = out[, 2] + 0.00071)
d = six_ion_MIXDER$d
#Then the 95% CI multivariate MC simulated ribbon
ninty_five_CI_lower = vector(length = 0)
ninty_five_CI_upper = vector(length = 0)
a = vector(length = 0)
for (i in 2:length(d)) {
  a = CI_function_MIXDER(d = c(0, six_ion_MIXDER$d[i]), d_interested = six_ion_MIXDER$d[i], r = rep(1/6, times = 6), L = c(75, 100, 125, 175, 195, 240), Z.beta = c(595, 690, 770, 1075, 1245, 1585))
  ninty_five_CI_lower = c(ninty_five_CI_lower, a[1])
  ninty_five_CI_upper = c(ninty_five_CI_upper, a[2])
}
six_ion_MIXDER$CI_lower = c(0, ninty_five_CI_lower + 0.00071)
six_ion_MIXDER$CI_upper = c(0, ninty_five_CI_upper + 0.00071)

#Then the 95% CI for the MC simulated independent gaussians
ninty_five_CI_lower_two = vector(length = 0)
ninty_five_CI_upper_two = vector(length = 0)
a = vector(length = 0)
for (i in 2:length(d)) {
  a = CI_function_MIXDER_MC_2(d = c(0, six_ion_MIXDER$d[i]), d_interested = six_ion_MIXDER$d[i], r = rep(1/6, times = 6), L = c(75, 100, 125, 175, 195, 240), Z.beta = c(595, 690, 770, 1075, 1245, 1585))
  ninty_five_CI_lower_two = c(ninty_five_CI_lower_two, a[1])
  ninty_five_CI_upper_two = c(ninty_five_CI_upper_two, a[2])
}
six_ion_MIXDER$CI_lower_two = c(0, ninty_five_CI_lower_two + 0.00071)
six_ion_MIXDER$CI_upper_two = c(0, ninty_five_CI_upper_two + 0.00071)

#Get the simple effect additivity MIXDER 
six_ion_MIXDER$simpleeffect = IDER(d = 1/6*d, L = 125, Z.beta = 770) + IDER(d = 1/6*d, L = 175, Z.beta = 1075) + IDER(d = 1/6*d, L = 75, Z.beta = 595) + IDER(d = 1/6*d, L = 100, Z.beta = 690) + IDER(d = 1/6*d, L = 195, Z.beta = 1245) + IDER(d = 1/6*d, L = 240, Z.beta = 1585) + 0.00071
#Get the individual IDERS
six_ion_MIXDER$oxygen = IDER(d = d, L = 75, Z.beta = 595) + 0.00071
six_ion_MIXDER$silicon = IDER(d = d, L = 100, Z.beta = 690) + 0.00071
six_ion_MIXDER$titanium = IDER(d = d, L = 125, Z.beta = 770) + 0.00071
six_ion_MIXDER$ironsix = IDER(d = d, L = 175, Z.beta = 1075) + 0.00071
six_ion_MIXDER$ironfour = IDER(d = d, L = 195, Z.beta = 1245) + 0.00071
six_ion_MIXDER$ironthree = IDER(d = d, L = 240, Z.beta = 1585) + 0.00071
save(six_ion_MIXDER, file = "six_ion_95%.Rda")

#plotting first figure for MC multivariate gaussian
fig_42ff_1 = ggplot(data = six_ion_MIXDER, aes(x = d*100, y = CA* 100)) + theme_bw() + theme(axis.line.x = element_line(color="black", size = 0.3), axis.line.y = element_line(color="black", size = 0.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_line(size = 0.3), axis.ticks.x = element_line(size = 0.3), axis.text=element_text(size=12), axis.title=element_text(size=14), axis.ticks.length = unit(0.3, "cm")) + labs(x = "Dose (cGy)", y = "CA (%)", title = "Mixture of 6 ions") + geom_ribbon(aes(ymin = CI_lower* 100, ymax = CI_upper * 100), fill = "grey90") + geom_line(aes(y = CA * 100), col = "red") + geom_line(aes(y = simpleeffect * 100), col = "black") + geom_line(aes(y = silicon* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = titanium* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironthree* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironfour* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironsix* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = oxygen* 100), col = "blue", linetype = "dotted") 

#Plotting second figure independent gaussian
fig_42ff_2 = ggplot(data = six_ion_MIXDER, aes(x = d*100, y = CA* 100)) + theme_bw() + theme(axis.line.x = element_line(color="black", size = 0.3), axis.line.y = element_line(color="black", size = 0.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_line(size = 0.3), axis.ticks.x = element_line(size = 0.3), axis.text=element_text(size=12), axis.title=element_text(size=14), axis.ticks.length = unit(0.3, "cm")) + labs(x = "Dose (cGy)", y = "CA (%)", title = "Mixture of 6 ions") + geom_ribbon(aes(ymin = CI_lower_two* 100, ymax = CI_upper_two * 100), fill = "grey75") + geom_line(aes(y = CA * 100), col = "red") + geom_line(aes(y = simpleeffect * 100), col = "black") + geom_line(aes(y = silicon* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = titanium* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironthree* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironfour* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = ironsix* 100), col = "blue", linetype = "dotted") + geom_line(aes(y = oxygen* 100), col = "blue", linetype = "dotted") 

setEPS()
postscript("fig_42ff.eps", width = 12, height = 4)
grid.arrange(fig_42ff_1, fig_42ff_2, ncol = 2)
dev.off()