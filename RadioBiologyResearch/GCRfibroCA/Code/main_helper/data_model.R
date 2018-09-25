#All the libraries needed for this script
library(ggplot2) #library for plotting
library(deSolve) # library for solving differential equations
library(gridExtra) #for plotting
library(minpack.lm) #for non-linear regression package

Oxygen = data.frame(d = c(0, .0125, .02, .025, .05, .075, .1, .2, .4), 
                    CA = c(.24, 1.66, 2.43, 2.37, 1.16, 2.85, 2.58, 6.94, 6.91))

Si = data.frame(d = c(0, .02, .04, .06, .08, .1, .12, .2, .4, .8, 1.2), 
                CA = c(.11, 1.26, 1.14, 1.58, 1.22, 1.89, 3.47, 4.6, 9.79, 27.01, 38.84))

Fe600 = data.frame(d = c(0, .01, .02, .04, .06, .08, .1, .12, .2, .4, .8), 
                   CA = c(.13, .76, .99, 1.2, 1.74, 1.28, 1.2, 1.7, 3.02, 5.52, 12.42))

Fe450 = data.frame(d = c(0, .02, .04, .06, .08, .1, .2, .4), 
                   CA = c(0, .86, .6, .8, 1.22, 2.02, 2.3, 4.77))

Fe300 = data.frame(d = c(0, .005, .01,  0.02, .04, .07, .1, .2, .4, .8), 
                   CA = c(0.41, 1.23, 1.47, 1.22, .97, 1.46, 1.21, 4.38, 6.22, 13.6))

Ti = data.frame(d = c(0,  0.02, .04, .06, .08, .1, .15, .3, .6), 
                CA = c(0, 1.99, 1.88, 1.44, 2.67, 2.57, 2.50, 5.64, 11.19))

param = data.frame(ion = c("O", "Si", "Ti", "Fe600", "Fe450", "Fe300"),
                   Z = c(8, 14, 22, 26, 26, 26), L = c(75, 100, 125, 175, 195, 240), 
                   Z.beta = c(595, 690, 770, 1075, 1245, 1585))

#putting it in one big data frame
big_df = rbind(Oxygen, Si, Ti, Fe600, Fe450, Fe300)
big_df$Z = rep(param$Z, times = c(9, 11, 9, 11, 8, 10))
big_df$Z.beta = rep(param$Z.beta, times = c(9, 11, 9, 11, 8, 10))
big_df$L = rep(param$L, times = c(9, 11, 9, 11, 8, 10))
big_df$error = c(0.24, 0.63, 0.77, 0.75, 0.52, 0.82, 0.78, 1.31, 1.59, 0.12, 0.05, 0.07, 0.56, 0.18, 0.60, 1.23, 1.60, 1.55, 4.27, 7.21, 0, 0.70, 0.66, 0.59, 0.80, 0.78, 0.48, 1.15, 2.39, 0.16, 0.38, 0.24, 0.21, 0.4, 0.37, 0.54, 0.17, 0.55, 1.75, 2.59, 0, 0.43, 0.34, 0.40, 0.50, 0.64, 0.73, 1.09, 0.29, 0.55, 0.60, 0.55, 0.49, 0.60, 0.54, 1.03, 1.22, 3.62)
big_df$ion = rep(param$ion, times = c(9, 11, 9, 11, 8, 10))

#will modify the data frame to get rid of the zero dose points irrelevant to our main parameter estimation
modified_df = big_df[big_df$d != 0, ]
modified_df$CA = modified_df$CA*0.01
modified_df$error = modified_df$error*0.01
big_df$CA = big_df$CA * 0.01
big_df$error = big_df$error * 0.01
big_df$errorbar_lower = big_df$CA - big_df$error
big_df$errorbar_upper = big_df$CA + big_df$error

#NTE1 function
NTE1_function = function(d, L, Z.beta, eta0 = 0.00011, eta1 = 0.007, sig0 = 6.12, kap = 796) {
  0.0017 + eta0*L*exp(-eta1*L)*(d != 0) + 
    (6.242*(d/L))*(sig0*(1-exp(-Z.beta/kap))^2 + 0.041/6.24*L*(1 - (1-exp(-Z.beta/kap))^2))
} 

#NTE2 function
NTE2_function = function(d, L, Z.beta, eta0 = 0.00047, eta1 = 0.011, sig0 = 6.75, kap = 590) {
  0.0017 + eta0*L*exp(-eta1*L)*exp(-(1012*(d/L)))*(d != 0) + 
    (6.242*(d/L))*(1-exp(-(1012*(d/L))))*
    (sig0*(1-exp(-Z.beta/kap))^2 + 0.041/6.24*L*(1 - (1-exp(-Z.beta/kap))^2))
} 
