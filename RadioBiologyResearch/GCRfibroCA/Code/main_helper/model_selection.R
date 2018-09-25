#Information criteria (AIC and BIC) - table 5 
#L_function gives the residuals squared
eta0 = as.vector(coef(IDER_model)[1]); eta1 = as.vector(coef(IDER_model)[2]); sig0 = as.vector(coef(IDER_model)[3]); kap = as.vector(coef(IDER_model)[4])
L_function = function(func, eta0, eta1, sig0, kap) {
  a = vector(length = 0)
  for (i in 1:length(modified_df[, 1])) {
    a = c(a, modified_df$CA[i] - func(d = modified_df$d[i], L = modified_df$L[i], Z.beta = modified_df$Z.beta[i], eta0 = eta0, eta1 = eta1, sig0 = sig0, kap = kap))
  }
  return(a^2)
}
L_NTE1 = L_function(NTE1_function, eta0 = 0.00011, eta1 = 0.007, sig0 = 6.12, kap = 796)
L_NTE2 = L_function(NTE2_function, eta0 = 0.00047, eta1 = 0.011, sig0 = 6.75, kap = 590)
L_IDER = L_function(IDER, eta0 = eta0, eta1 = eta1, sig0 = sig0, kap = kap)

#Since all models both used in our NASA report and 16Cacao's are weighted least square regression, will weight it with our weights to get the WRSS (weighted residual squared sum)
WRSS_NTE1 = sum((1/modified_df$error^2)*L_NTE1)
WRSS_NTE2 = sum((1/modified_df$error^2)*L_NTE2)
WRSS_IDER = sum((1/modified_df$error^2)*L_IDER)

#Definition of AIC and BIC calculation for Weighted Least Square regression
AIC_function = function(RSS, k = 4, n = length(modified_df[ , 1])) {
  n + n*log(2*pi) + n*log(RSS/n) + 2*(k+1)
}

BIC_function = function(n = length(modified_df[, 1]), k = 4, RSS) {
  n + n*log(2*pi) + n*log(RSS/n) + log(n)*(k+1)
}

NTE1_AIC = AIC_function(RSS = WRSS_NTE1)
NTE2_AIC = AIC_function(RSS = WRSS_NTE2)
IDER_AIC = AIC_function(RSS = WRSS_IDER)
NTE1_BIC = BIC_function(RSS = WRSS_NTE1)
NTE2_BIC = BIC_function(RSS = WRSS_NTE2)
IDER_BIC = BIC_function(RSS = WRSS_IDER)
information_critera_df = data.frame(AIC = c(NTE1_AIC, NTE2_AIC, IDER_AIC), BIC = c(NTE1_BIC, NTE2_BIC, IDER_BIC), row.names = c("NTE1 model", "NTE2 model", "IDER model"))
information_critera_df
