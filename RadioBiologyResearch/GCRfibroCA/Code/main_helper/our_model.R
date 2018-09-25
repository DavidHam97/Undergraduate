#SNTE IDER, Tables 2-4 in NASA report
library("minpack.lm")
#our proposed function
IDER = function(d, L, Z.beta, eta0, eta1, sig0, kap) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  eta = eta0*L*exp(-eta1*L)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d))
} 

#nls method to get the parameters needed (4 parameter estimation)
IDER_model = nls(CA ~ IDER(d, L, Z.beta, eta0, eta1, sig0, kap), data = modified_df, start = list(eta0 = 0.001, eta1 = 0.01, sig0 = 5, kap = 500), 
                 weights = (1/(modified_df$error)^2))
coef(IDER_model)
vcov(IDER_model)
summary(IDER_model, cor = TRUE)