##Expansion Modelling Supplementary Section 2
#Oxygen Individual Fit
IDER_oxygen = function(d, L, eta0, sig0) {
  0.0024 + sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

oxygen_df = big_df[1:9, ]
oxygen_df_w0 = oxygen_df[-1, ]
oxygen_model = nls(CA ~ IDER_oxygen(d, L, eta0, sig0), data = oxygen_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
                   weights = (1/(oxygen_df_w0$error)^2))
summary(oxygen_model)

#Si Individaul Fit
si_df = big_df[big_df[, 7] == "Si", ]
si_df_w0 = si_df[-1, ]
si_df[1, 2] #0.0011 is the intercept
IDER_si= function(d, L, eta0, sig0) {
  0.0011 + sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

si_model = nls(CA ~ IDER_si(d, L, eta0, sig0), data = si_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
               weights = (1/(si_df_w0$error)^2))
summary(si_model)

#Ti individual fit
ti_df = big_df[big_df[, 7] == "Ti", ]
ti_df_w0 = ti_df[-1, ]
ti_df[1, 2] #0.0011 is the intercept
IDER_ti= function(d, L, eta0, sig0) {
  sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

ti_model = nls(CA ~ IDER_ti(d, L, eta0, sig0), data = ti_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
               weights = (1/(ti_df_w0$error)^2))
summary(ti_model)

#Fe_600 individual fit
fe600_df = big_df[big_df[, 7] == "Fe600", ]
fe600_df_w0 = fe600_df[-1, ]
fe600_df[1, 2] #0.0013 is the intercept
IDER_fe600= function(d, L, eta0, sig0) {
  0.0013 + sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

fe600_model = nls(CA ~ IDER_fe600(d, L, eta0, sig0), data = fe600_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
                  weights = (1/(fe600_df_w0$error)^2))

#Fe_450 individual fit
fe450_df = big_df[big_df[, 7] == "Fe450", ]
fe450_df_w0 = fe450_df[-1, ]
fe450_df[1, 2] 
IDER_fe450= function(d, L, eta0, sig0) {
  sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

fe450_model = nls(CA ~ IDER_fe450(d, L, eta0, sig0), data = fe450_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
                  weights = (1/(fe450_df_w0$error)^2))
summary(fe450_model)

#Fe_300 individual fit
fe300_df = big_df[big_df[, 7] == "Fe300", ]
fe300_df_w0 = fe300_df[-1, ]
fe300_df[1, 2] 
IDER_fe300= function(d, L, eta0, sig0) {
  0.0041 + sig0*d*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d))
}

fe300_model = nls(CA ~ IDER_fe300(d, L, eta0, sig0), data = fe300_df_w0, start = list(eta0 = 0.01, sig0 = 1), 
                  weights = (1/(fe300_df_w0$error)^2))
summary(fe300_model)

#Graphing individual ion fit 
#Just 2 parameter and 4 parameter comparison
oxygen_comp = ggplot() + geom_line(aes(x = d_oxygen, y = 100*original_IDER(d = d_oxygen, L = 75, Z.beta= 595)), col = "red") + geom_line(aes(x = d_oxygen, y = 100*two_IDER(d = d_oxygen, L = 75)), col = "blue") + geom_line(aes(x = d_oxygen, y = 100*IDER_oxygen(d = d_oxygen, eta0 = coef(oxygen_model)[1], sig0 = coef(oxygen_model)[2],L = 75)), col = "orange") + labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Oxygen") + geom_errorbar(data = big_df[big_df[, 7] == "O", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "O", ], aes(x = d, y = 100*CA), size = 2) + annotate("text",x = 0.05, y = 8.5, label = "atop(bold('A'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

si_comp = ggplot() + geom_line(aes(x = d_si, y = 100*original_IDER(d = d_si, L = 100, Z.beta= 690)), col = "red") + geom_line(aes(x = d_si, y = 100*two_IDER(d = d_si, L = 100)), col = "blue") + geom_line(aes(x = d_si, y = 100*IDER_si(d = d_si, eta0 = coef(si_model)[1], sig0 = coef(si_model)[2],L = 100)), col = "orange") + labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Silicon") + geom_errorbar(data = big_df[big_df[, 7] == "Si", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "Si", ], aes(x = d, y = 100*CA), size = 2) + xlim(c(0, 0.4)) + ylim(c(0, 12.5)) + annotate("text",x = 0.05, y = 9, label = "atop(bold('B'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

ti_comp = ggplot() + geom_line(aes(x = d_ti, y = 100*original_IDER(d = d_ti, L = 125, Z.beta= 770)), col = "red") + geom_line(aes(x = d_ti, y = 100*two_IDER(d = d_ti, L = 125)), col = "blue")+ geom_line(aes(x = d_ti, y = 100*IDER_ti(d = d_ti, eta0 = coef(ti_model)[1], sig0 = coef(ti_model)[2],L = 125)), col = "orange") + labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Titanium") + geom_errorbar(data = big_df[big_df[, 7] == "Ti", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "Ti", ], aes(x = d, y = 100*CA), size = 2) + xlim(c(0, 0.4)) + ylim(c(0, 7.5)) + annotate("text",x = 0.05, y = 5.5, label = "atop(bold('C'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

fe300_comp = ggplot() + geom_line(aes(x = d_fe300, y = 100*original_IDER(d = d_fe300, L = 175, Z.beta= 1075)), col = "red") + geom_line(aes(x = d_fe300, y = 100*two_IDER(d = d_fe300, L = 175)), col = "blue") + geom_line(aes(x = d_fe300, y = 100*IDER_fe300(d = d_fe300, eta0 = coef(fe300_model)[1], sig0 = coef(fe300_model)[2],L = 175)), col = "orange") + labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Fe0.65") + geom_errorbar(data = big_df[big_df[, 7] == "Fe300", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "Fe300", ], aes(x = d, y = 100*CA), size = 2) + xlim(c(0, 0.4)) + ylim(c(0, 7.5)) + annotate("text",x = 0.05, y = 5.5, label = "atop(bold('D'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

fe450_comp = ggplot() + geom_line(aes(x = d_fe450, y = 100*original_IDER(d = d_fe450, L = 195, Z.beta= 1245)), col = "red") + geom_line(aes(x = d_fe450, y = 100*two_IDER(d = d_fe450, L = 195)), col = "blue") + geom_line(aes(x = d_fe450, y = 100*IDER_fe450(d = d_fe450, eta0 = coef(fe450_model)[1], sig0 = coef(fe450_model)[2],L = 195)), col = "orange") +  labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Fe0.74") + geom_errorbar(data = big_df[big_df[, 7] == "Fe450", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "Fe450", ], aes(x = d, y = 100*CA), size = 2) + annotate("text",x = 0.05, y = 4.5, label = "atop(bold('E'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

fe600_comp = ggplot() + geom_line(aes(x = d_fe600, y = 100*original_IDER(d = d_fe600, L = 240, Z.beta= 1585)), col = "red") + geom_line(aes(x = d_fe600, y = 100*two_IDER(d = d_fe600, L = 240)), col = "blue")+ labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "Fe0.79") + geom_line(aes(x = d_fe600, y = 100*IDER_fe600(d = d_fe600, eta0 = coef(fe600_model)[1], sig0 = coef(fe600_model)[2],L = 240)), col = "orange")  +  geom_errorbar(data = big_df[big_df[, 7] == "Fe600", ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0) + geom_point(data = big_df[big_df[, 7] == "Fe600", ], aes(x = d, y = 100*CA), size = 2) + xlim(c(0, 0.4)) + ylim(c(0, 7.5)) + annotate("text",x = 0.05, y = 5.5, label = "atop(bold('F'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), plot.title =element_text(size=22), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20))

pdf("IDER_fig_expansaion.pdf", width = 9, height = 7)
grid.arrange(oxygen_comp, si_comp, ti_comp, fe300_comp, fe450_comp, fe600_comp, ncol = 3)
dev.off()

#CV Error for twelve parameter model
vec = c(sum(residuals(oxygen_model)^2), sum(residuals(si_model)^2), sum(residuals(ti_model)^2), sum(residuals(fe300_model)^2), sum(residuals(fe450_model)^2), sum(residuals(fe600_model)^2))
cv_error_twelve = mean(vec)
cv_error_twelve