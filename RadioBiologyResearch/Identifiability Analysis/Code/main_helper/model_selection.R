#AIC/BIC Tables
original_AIC = AIC(model_original); original_BIC = BIC(model_original)

three_no_kap_AIC = AIC(model_three_no_kap); three_no_kap_BIC = BIC(model_three_no_kap)

three_no_eta1_AIC = AIC(model_three_no_eta1); three_no_eta1_BIC = BIC(model_three_no_eta1)

two_AIC = AIC(model_two); two_BIC = BIC(model_two)

TE_AIC = AIC(model_TE); TE_BIC = BIC(model_TE)

matrix(c(original_AIC, three_no_kap_AIC, three_no_eta1_AIC, two_AIC, TE_AIC ,original_BIC, three_no_kap_BIC, three_no_eta1_BIC, two_BIC, TE_BIC), nrow = 5, dimnames = list(c("Original 4 Parameter", "3 Parameter no Kap", "Three Parameter no eta1", "Two Parameter", "TE Only"), c("AIC", "BIC")))

#Table 4.1.1: Cross Validation - 6 fold
#Measure of fit is using Median absolute deviation known. 

ion = unique(modified_df$ion)
#CV for 3 parameter no kap
cv_error_three_no_kap <- function(data = modified_df) {
  theoretical = vector()
  observed = vector()
  for (i in 1:6) {
    train_data = modified_df[!modified_df$ion == ion[i], ]
    test_data = modified_df[modified_df$ion == ion[i], ]
    fit = nls(CA ~ three_IDER_no_kap(d, L, eta0, eta1, sig0), data = train_data, start = list(eta0 = 0.0001, eta1 = 0.01, sig0 = 1), weights = (1/(train_data$error))^2)
    predic = predict(fit, test_data)
    actual = test_data$CA
    theoretical = c(theoretical, predic)
    observed = c(observed, actual)
  }
  error = median(abs(theoretical - observed))
  return(error)
}

cv_errors_three_no_kap = cv_error_three_no_kap()

#CV for 3 parameter no eta1
cv_error_three_no_eta1 <- function(data = modified_df) {
  theoretical = vector()
  observed = vector()
  for (i in 1:5) {
    train_data = modified_df[!modified_df$ion == ion[i], ]
    test_data = modified_df[modified_df$ion == ion[i], ]
    fit = nls(CA ~ three_IDER_no_eta1(d, L, Z.beta, eta0, sig0, kap), data = train_data, start = list(eta0 = 0.001, sig0 = 1, kap = 500), weights = (1/(train_data$error))^2)
    predic = predict(fit, test_data)
    actual = test_data$CA
    theoretical = c(theoretical, predic)
    observed = c(observed, actual)
  }
  error = median(abs(theoretical - observed))
  return(error)
}

cv_errors_three_no_eta1 = cv_error_three_no_eta1()

#Cross validation for two parameter
cv_error_two <- function(data = modified_df) {
  theoretical = vector()
  observed = vector()
  for (i in 1:6) {
    train_data = modified_df[!modified_df$ion == ion[i], ]
    test_data = modified_df[modified_df$ion == ion[i], ]
    fit = nls(CA ~ two_IDER(d, L, eta0, sig0), data = train_data, start = list(eta0 = 0.05, sig0 = 1), weights = (1/(train_data$error))^2)
    predic = predict(fit, test_data)
    actual = test_data$CA
    theoretical = c(theoretical, predic)
    observed = c(observed, actual)
  }
  error = median(abs(theoretical - observed))
  return(error)
}

cv_errors_two = cv_error_two()

#Cross validation for TE model
cv_error_TE <- function(data = modified_df) {
  sum_error = vector(length = 5)
  theoretical = vector()
  observed = vector()
  for (i in 2:6) {
    train_data = modified_df[!modified_df$ion == ion[i], ]
    test_data = modified_df[modified_df$ion == ion[i], ]
    fit = nls(CA ~ TE_IDER(d, L, kap, sig0 ,Z.beta), data = train_data, start = list(kap = 500, sig0 = 1), weights = (1/(train_data$error))^2)
    predic = predict(fit, test_data)
    actual = test_data$CA
    theoretical = c(theoretical, predic)
    observed = c(observed, actual)
  }
  error = median(abs(theoretical - observed))
  return(error)
}

cv_errors_TE = cv_error_TE()

#Cross validation for original model
cv_error_originalmodel <- function(data = modified_df) {
  theoretical = vector()
  observed = vector()
  for (i in 1:6) {
    train_data = modified_df[!modified_df$ion == ion[i], ]
    test_data = modified_df[modified_df$ion == ion[i], ]
    fit = nls(CA ~ original_IDER(d, L, Z.beta, eta0, eta1, sig0, kap), data = modified_df, start = list(eta0 = 0.001, eta1 = 0.01, sig0 = 5, kap = 500), weights = (1/(modified_df$error)^2))
    predic = predict(fit, test_data)
    actual = test_data$CA
    theoretical = c(theoretical, predic)
    observed = c(observed, actual)
  }
  error = median(abs(theoretical - observed))
  return(error)
}
cv_errors_original = cv_error_originalmodel()

matrix(c(cv_errors_three_no_eta1, cv_errors_three_no_kap, cv_errors_two, cv_errors_TE ,cv_errors_original), nrow = 1, dimnames = list(c("CV Error"), c("Three Parameters No Eta1", "Three Parameters No Kap", "Two Parameters", "TE Model","Original Four Parameters")))