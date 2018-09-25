#Our Original 4 Parameter Model: Section 2.3
original_IDER = function(d, L, Z.beta, eta0, eta1, sig0, kap) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  eta = eta0*L*exp(-eta1*L)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d))
} 

#nls method to get the parameters needed (4 parameter estimation)
model_original = nls(CA ~ original_IDER(d, L, Z.beta, eta0, eta1, sig0, kap), data = modified_df, start = list(eta0 = 0.001, eta1 = 0.01, sig0 = 5, kap = 500), 
                     weights = (1/(modified_df$error)^2))
summary(model_original, cor = TRUE)

#Introducting Parsimonious Models: Section 3.1-3.4
#3 parameter model no kap
three_IDER_no_kap = function(d, L, eta0, eta1, sig0) {
  eta = eta0*L*exp(-eta1*L)
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d))
}
model_three_no_kap = nls(CA ~ three_IDER_no_kap(d, L, eta0, eta1, sig0), data = modified_df, start = list(eta0 = 0.0001, eta1 = 0.01, sig0 = 1), weights = (1/(modified_df$error))^2)

#3 parameter model no eta1
three_IDER_no_eta1 = function(d, L, Z.beta, eta0, sig0, kap) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
} 
model_three_no_eta1 = nls(CA ~ three_IDER_no_eta1(d, L, Z.beta, eta0, sig0, kap), data = modified_df, start = list(eta0 = 0.001, sig0 = 1, kap = 500), weights = (1/(modified_df$error))^2)

#The 2 parameter IDER with no eta1 and no kap
two_IDER = function(d, L, eta0, sig0) {
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}
model_two = nls(CA ~ two_IDER(d, L, eta0, sig0), data = modified_df, start = list(eta0 = 0.05, sig0 = 1), weights = (1/(modified_df$error))^2)

#The 2 parameter IDER with no etas at all
TE_IDER = function(d, L, kap, sig0, Z.beta) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L))
}
model_TE = nls(CA ~ TE_IDER(d, L, kap, sig0, Z.beta), data = modified_df, start = list(kap = 500, sig0 = 1), weights = (1/(modified_df$error))^2)

#The 2 parameter IDER with no etas at all
TE_one_IDER = function(d, L, sig0) {
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L))
}
model_one_TE = nls(CA ~ TE_one_IDER(d, L, sig0), data = modified_df, start = list(sig0 = 1), weights = (1/(modified_df$error))^2)

#All the models with fitted parameters: Section 3.1-3.4
original_IDER = function(d, L, Z.beta, eta0 = coef(model_original)[1], eta1 = coef(model_original)[2], sig0 = coef(model_original)[3], kap = coef(model_original)[4]) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  eta = eta0*L*exp(-eta1*L)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d))
} 

three_IDER_no_kap = function(d, L, eta0 = coef(model_three_no_kap)[1], eta1 = coef(model_three_no_kap)[2], sig0 = coef(model_three_no_kap)[3]) {
  eta = eta0*L*exp(-eta1*L)
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d))
}

three_IDER_no_eta1 = function(d, L, Z.beta, eta0 = coef(model_three_no_eta1)[1], sig0 = coef(model_three_no_eta1)[2], kap = coef(model_three_no_eta1)[3]) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
} 

two_IDER = function(d, L, eta0 = coef(model_two)[1], sig0 = coef(model_two)[2]) {
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

TE_IDER = function(d, L, kap = coef(model_TE)[1], sig0 = coef(model_TE)[2], Z.beta) {
  P = (1-exp(-Z.beta/kap))^2
  sig = sig0*P + 0.041/6.24*L*(1-P)
  0.00071 + sig*6.24*d/L*(1-exp(-1024*d/L))
}

TE_one_IDER = function(d, L, sig0 = coef(model_one_TE)[1]) {
  0.00071 + sig0*6.24*d/L*(1-exp(-1024*d/L))
}

#Creating doses
d_oxygen = seq(0, 0.4, 0.001)
d_si = seq(0, 1.2, 0.001)
d_ti = seq(0, 0.6, 0.001)
d_fe600 = seq(0, 0.8, 0.001)
d_fe450 = seq(0, 0.4, 0.001)
d_fe300 = seq(0, 0.8, 0.001)

#Parameter correlation comparison: Tables 3.1.1-3.4.1
#Original model
cov2cor(vcov(model_original))

#3 parameter without kap
cov2cor(vcov(model_three_no_kap))

#3 parmater without eta1
cov2cor(vcov(model_three_no_eta1))

#2 parameter without kap and eta1
cov2cor(vcov(model_two))

#2 parameter without NTE
cov2cor(vcov(model_TE))

#Summary of fits: Tables 3.1.1-3.4.1
summary(model_original)
summary(model_three_no_kap)
summary(model_three_no_eta1)
summary(model_two)
summary(model_TE)